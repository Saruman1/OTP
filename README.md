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
