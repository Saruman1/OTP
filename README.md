# OTP w Erlang: krótkie wprowadzenie

## Cel tego repozytorium

To repozytorium zawiera prosty przykład serwera w Erlangu oraz jego stopniową refaktoryzację w kierunku wzorca, na którym opiera się **OTP `gen_server`**.

Ten plik `README.md` pełni jednocześnie rolę:

- krótkiego wprowadzenia teoretycznego,
- dokumentacji kodu,
- **scenariusza prezentacji (krok po kroku)** – na końcu znajdziesz dokładny plan, jak o tym opowiadać na slajdach / na żywo.

---

## Czym jest OTP?

**OTP (Open Telecom Platform)** to standardowe biblioteki i zestaw wzorców, które pomagają budować:

- współbieżne,
- rozproszone,
- odporne na błędy

systemy w Erlangu.

Najważniejsze idee:

- Erlang ma bardzo lekkie i tanie w utrzymaniu procesy.
- Ale jeśli za każdym razem ręcznie piszemy:
  - `spawn`, `spawn_link`
  - monitory (`erlang:monitor/2`), linki
  - pętle `receive`
  - obsługę błędów, zakończenia procesu, timeouty
  – łatwo popełnić błąd.

**OTP** dostarcza gotowych **behaviours** (np. `gen_server`, `gen_statem`, `supervisor`), które ukrywają całą tę powtarzalną logikę.

Najważniejszy dla prostych serwerów jest **`gen_server`**.

W tym repozytorium chcemy zrozumieć ideę:

> najpierw napiszemy naiwne rozwiązanie,
> potem wyciągniemy część wspólną do modułu `my_server`,
> a na końcu zobaczymy, że to właśnie robi `gen_server`.

---

## Wzorzec „podstawowego serwera”

Większość serwerów w Erlangu wygląda podobnie:

1. **start/init**
   - uruchamiamy nowy proces (`spawn` / `spawn_link`),
   - ustawiamy stan początkowy.
2. **loop(State)**
   - nieskończona pętla `receive`,
   - odbieramy wiadomość,
   - aktualizujemy stan,
   - wywołujemy ponownie `loop(NewState)`.
3. **dodatkowa logika**
   - monitory,
   - timeouty,
   - obsługa zakończenia procesu,
   - logowanie błędów itd.

Ten wzorzec powtarza się **w każdej** implementacji serwera. To oznacza, że możemy go zautomatyzować i zamienić w bibliotekę.

---

## Krok 1. Naiwny `kitty_server` (bez OTP)

Zbudujmy prosty **sklep z kotami**:

- **wywołanie synchroniczne**: zamów kota (`order_cat/4`)
- **wywołanie asynchroniczne**: zwróć kota (`return_cat/2`)
- **wywołanie synchroniczne**: zamknij sklep (`close_shop/1`)

```erlang
%%%%% Naiwna wersja
-module(kitty_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color = green, description}).

%%% API klienta

start_link() ->
    spawn_link(fun init/0).

%% Wywołanie synchroniczne
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% Wywołanie asynchroniczne (fire-and-forget)
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%% Wywołanie synchroniczne – poprawne zakończenie pracy serwera
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%%% Logika serwera

init() ->
    loop([]).

loop(Cats) ->
    receive
        %% Zamówienie kota
        {Pid, Ref, {order, Name, Color, Description}} ->
            if
                Cats =:= [] ->
                    Pid ! {Ref, make_cat(Name, Color, Description)},
                    loop(Cats);
                Cats =/= [] ->
                    %% Najpierw oddajemy koty ze "stanu magazynowego"
                    Pid ! {Ref, hd(Cats)},
                    loop(tl(Cats))
            end;

        %% Ktoś zwrócił kota
        {return, Cat = #cat{}} ->
            loop([Cat | Cats]);

        %% Zamknięcie sklepu
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);

        %% Nieznana wiadomość
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

%%% Funkcje prywatne

make_cat(Name, Col, Desc) ->
    #cat{name = Name, color = Col, description = Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    ok.
```
Przykładowe użycie w shellu:
```
1> c(kitty_server).
{ok,kitty_server}
2> rr(kitty_server).
[cat]
3> Pid = kitty_server:start_link().
<0.57.0>
4> Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
#cat{name = carl, color = brown,
      description = "loves to burn bridges"}
5> kitty_server:return_cat(Pid, Cat1).
ok
6> kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
%% wróci carl ze "stanu magazynowego"
#cat{name = carl, color = brown,
      description = "loves to burn bridges"}
7> kitty_server:close_shop(Pid).
carl was set free.
ok
```
Problem:
Widzimy, że za każdym razem:

monitorujemy procesy,

implementujemy protokół wiadomości,

piszemy pętlę receive,

obsługujemy timeouty i błędy.

To powtarzalna, generyczna część. Można ją przenieść do wspólnej biblioteki.

## Krok 2. Wyodrębnienie części wspólnej – `my_server`

W kroku drugim wyciągamy wspólną, powtarzalną część logiki do osobnego modułu my_server.
Ten moduł nie wie nic o kotach – jego zadanie to bycie **małym frameworkiem do serwerów**.

Stworzymy moduł `my_server`, który:

uruchamia proces,

przechowuje i obsługuje stan w pętli `receive`,

udostępnia:

`call/2` – wywołania synchroniczne,

`cast/2` – wywołania asynchroniczne,

`reply/2` – odpowiedź na `call/2`,

wywołuje funkcje callback w module użytkownika:

`init/1`,

`handle_call/3`,

`handle_cast/2`.

```erlang
-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Publiczne API

start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

%% Wywołanie synchroniczne
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% Wywołanie asynchroniczne
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

%% Odpowiedź dla synchronicznego wywołania
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Wewnętrzna logika

init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} ->
            loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.
```

## Krok 3. `kitty_server2` jako moduł callback

Teraz serwer kotów staje się prosty i przejrzysty — zawiera tylko logikę domenową.

```erlang
-module(kitty_server2).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color = green, description}).

%%% API klienta

start_link() ->
    my_server:start_link(?MODULE, []).

order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
    my_server:call(Pid, terminate).

%%% Funkcje callback dla my_server

init([]) ->
    [].

handle_call({order, Name, Color, Description}, From, Cats) ->
    case Cats of
        [] ->
            my_server:reply(From, make_cat(Name, Color, Description)),
            Cats;
        [Cat | Rest] ->
            my_server:reply(From, Cat),
            Rest
    end;

handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat | Cats].

%%% Funkcje prywatne

make_cat(Name, Col, Desc) ->
    #cat{name = Name, color = Col, description = Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    exit(normal).

```
To, co zbudowaliśmy (`my_server` + moduł callback), jest uproszczonym modelem tego, co zapewnia `OTP` `gen_server`:

`gen_server:start_link/3,4` odpowiada `my_server:start_link/2`,

`gen_server:call/2,3` odpowiada `my_server:call/2`,

`gen_server:cast/2` odpowiada `my_server:cast/2`,

moduł callback implementuje:

`init/1`

`handle_call/3`

`handle_cast/2`

`handle_info/2`

`terminate/2`

`code_change/3`

## Idea OTP:

Oddzielić część generyczną (niezmienną) od części specyficznej (logika aplikacyjna).
Dzięki temu kod jest stabilniejszy, prostszy i mniej podatny na błędy.

## Podsumowanie

OTP to zestaw narzędzi upraszczających pisanie współbieżnych aplikacji w Erlangu.

Wzorzec serwera → powtarzalny → łatwo go ustandaryzować.

`my_server` jest uproszczonym modelem `gen_server`.

## Rozdzielenie logiki:

generyczna → wspólny moduł (`my_server`)

specyficzna → logika biznesowa (`kitty_server2`)

W praktycznych projektach — używamy gen_server + supervisorów.

```
1> c(my_server).
{ok,my_server}
2> c(kitty_server2).
{ok,kitty_server2}
```

```
3> Pid = kitty_server2:start_link().
<0.90.0>
```
Wywołuję `kitty_server2`:`start_link/0`.
Pod spodem to tak naprawdę `my_server`:`start_link(?MODULE, [])`.
Dzięki temu startuje nowy proces serwera, a jako callback używany jest moduł `kitty_server2`.
Stan początkowy to pusta lista kotów.
```
4> Cat1 = kitty_server2:order_cat(Pid, carl, brown, "loves bridges").
#cat{name = carl,color = brown,
      description = "loves bridges"}
```
Wywołuję `order_cat/4`, które pod spodem robi `my_server`:`call(Pid, {order, ...})`.
my_server wysyła wiadomość synchroniczną do serwera i czeka na odpowiedź.
W `handle_call/3` widzimy, że jeśli lista kotów jest pusta, to tworzony jest nowy kot i zwracany do klienta.
```
5> kitty_server2:return_cat(Pid, Cat1).
ok
```
Teraz wywołuję `return_cat/2`. To jest tylko `my_server`:`cast(Pid, {return, Cat})`.
cast jest asynchroniczny: klient nie czeka na odpowiedź, tylko informuje serwer: ten kot wrócił do magazynu.
W `handle_cast/2` logika jest prosta – dodajemy kota na początek listy.
```

6> Cat2 = kitty_server2:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = carl,color = brown,
      description = "loves bridges"}
```
Chociaż zamawiam kota `jimmy, orange, cuddly`, serwer oddaje mi `carl, brown, loves bridges`.
Dlaczego?
Bo logika w `handle_call/3` mówi: jeśli mamy kota na liście, to pierwszeństwo ma kot ze stanu magazynowego, a nie nowe zamówienie.
To pokazuje, że stan (Cats) jest poprawnie przechowywany i modyfikowany w `my_server:loop/2`.
```
7> Cat3 = kitty_server2:order_cat(Pid, jimmy, orange, "cuddly").
#cat{name = jimmy,color = orange,
      description = "cuddly"}
```
Wcześniej zdjęliśmy jednego kota z listy, więc magazyn znowu jest pusty.
Przy kolejnym zamówieniu serwer tworzy teraz nowego kota zgodnie z parametrami zamówienia.
```
8> kitty_server2:close_shop(Pid).
carl was set free.
ok
```
`close_shop/1` robi `my_server`:`call(Pid, terminate)`.
W `handle_call(terminate, ...)` wysyłamy klientowi ok, a następnie wołamy `terminate/1`, które wypisuje imiona wszystkich kotów i kończy proces `exit(normal)`.
```
9> kitty_server2:order_cat(Pid, x, black, "after close").
** exception error: no such process or port
```
Proces serwera już nie istnieje – sklep jest zamknięty, więc każde kolejne wywołanie na starym PID kończy się błędem.

### Krok 4 – prawdziwy gen_server: kitty_gen_server

Plik: `kitty_gen_server.erl`

```erl
-module(kitty_gen_server).

-behaviour(gen_server).

%% API publiczne
-export([start_link/0,
         order_cat/3,
         return_cat/1,
         close_shop/0]).

%% Callbacks gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(cat, {name, color = green, description}).

%%% ====================================================================
%%%  API
%%% ====================================================================

%% Ten start_link będzie wywoływany przez supervisora
start_link() ->
    %% rejestrujemy proces pod nazwą ?MODULE = kitty_gen_server
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Klient nie musi znać Pid – wystarczy nazwa modułu
order_cat(Name, Color, Description) ->
    gen_server:call(?MODULE, {order, Name, Color, Description}).

return_cat(Cat = #cat{}) ->
    gen_server:cast(?MODULE, {return, Cat}).

close_shop() ->
    gen_server:call(?MODULE, terminate).

%%% ====================================================================
%%%  Callbacks
%%% ====================================================================

%% Stan początkowy – lista kotów w „magazynie”
init([]) ->
    io:format("kitty_gen_server init~n", []),
    {ok, []}.

%% -------------------- handle_call/3 --------------------

handle_call({order, Name, Color, Description}, _From, Cats) ->
    case Cats of
        [] ->
            %% nie ma kotów na magazynie – tworzymy nowego
            Cat = make_cat(Name, Color, Description),
            {reply, Cat, Cats};
        [Cat | Rest] ->
            %% jest kot na magazynie – oddajemy tego, którego mamy
            {reply, Cat, Rest}
    end;

handle_call(terminate, _From, Cats) ->
    terminate_cats(Cats),
    %% {stop, Reason, Reply, NewState}
    {stop, normal, ok, Cats};

handle_call(Unknown, _From, State) ->
    io:format("Unknown call: ~p~n", [Unknown]),
    {reply, {error, unknown_call}, State}.

%% -------------------- handle_cast/2 --------------------

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat | Cats]};

handle_cast(Unknown, State) ->
    io:format("Unknown cast: ~p~n", [Unknown]),
    {noreply, State}.

%% -------------------- handle_info/2 --------------------

handle_info(Msg, State) ->
    io:format("Unhandled info: ~p~n", [Msg]),
    {noreply, State}.

%% -------------------- terminate/2 --------------------

terminate(Reason, Cats) ->
    io:format("kitty_gen_server terminating, reason=~p~n", [Reason]),
    terminate_cats(Cats),
    ok.

%% -------------------- code_change/3 --------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ====================================================================
%%%  Funkcje prywatne
%%% ====================================================================

make_cat(Name, Col, Desc) ->
    #cat{name = Name, color = Col, description = Desc}.

terminate_cats(Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats].

```

```
1> c(kitty_gen_server).
{ok,kitty_gen_server}

%% Ręczny start bez supervisora (na chwilę):
2> kitty_gen_server:start_link().
{ok,<0.90.0>}

3> Cat1 = kitty_gen_server:order_cat(carl, brown, "loves fish").
#cat{name = carl,color = brown,description = "loves fish"}

4> kitty_gen_server:return_cat(Cat1).
ok

5> kitty_gen_server:order_cat(jimmy, orange, "cuddly").
%% wróci carl z magazynu:
#cat{name = carl,color = brown,description = "loves fish"}

6> kitty_gen_server:close_shop().
carl was set free.
ok
```

Teraz robimy to samo, co w `kitty_server2`, ale używając prawdziwego `gen_server`.
Widzimy, że API jest bardzo podobne, ale callbacki zwracają krotki `{reply, ...}`, `{noreply, ...}`, `{stop, ...}`, a całe zarządzanie procesem leży po stronie OTP.

## Krok 5 – supervisor dla `kitty_gen_server`: `kitty_sup`
Teraz chcemy wpiąć nasz serwer do drzewa supervisorów.

Plik: `kitty_sup.erl`
```erl
-module(kitty_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%% ====================================================================
%%%  API
%%% ====================================================================

start_link() ->
    %% Rejestrujemy supervisora pod nazwą kitty_sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% ====================================================================
%%%  Callbacks supervisor
%%% ====================================================================

init([]) ->
    %% Strategia restartu: one_for_one
    SupFlags = {one_for_one,   % strategia
                5,             % MaxRestarts
                10},           % MaxSecondsBetweenRestarts

    %% Specyfikacja dziecka (nasz gen_server)
    ChildSpec =
        {kitty_gen_server,                       % Id
         {kitty_gen_server, start_link, []},     % {M, F, A}
         permanent,                              % Restart
         5000,                                   % Shutdown (ms)
         worker,                                 % Typ
         [kitty_gen_server]},                    % Moduły

    {ok, {SupFlags, [ChildSpec]}}.

```

```
1> c(kitty_gen_server).
{ok,kitty_gen_server}
2> c(kitty_sup).
{ok,kitty_sup}

%% Startujemy supervisora
3> kitty_sup:start_link().
{ok,<0.95.0>}

%% Supervisor automatycznie startuje kitty_gen_server,
%% zarejestrowany pod nazwą kitty_gen_server

4> kitty_gen_server:order_cat(carl, brown, "from supervisor").
#cat{name = carl,color = brown,description = "from supervisor"}
```
Automatyczny restart:
```
%% Sprawdzamy Pid procesu serwera
5> whereis(kitty_gen_server).
<0.97.0>

%% Zabijamy go ręcznie:
6> exit(whereis(kitty_gen_server), kill).
true

%% Sprawdzamy znowu:
7> whereis(kitty_gen_server).
<0.99.0>
```
Widzimy, że Pid się zmienił – stary proces został zabity,
ale supervisor uruchomił nowy proces `kitty_gen_server`.
To jest cała moc supervision tree: procesy mogą padać, a system się sam podnosi.

Rowniez stan się zresetował:
```
8> Cat = kitty_gen_server:order_cat(x, black, "after restart").
#cat{name = x,color = black,description = "after restart"}
```
