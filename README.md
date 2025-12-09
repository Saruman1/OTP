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
