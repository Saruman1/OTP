# OTP w Erlang: krótkie wprowadzenie

## Czym jest OTP?

**OTP (Open Telecom Platform)** to standardowe biblioteki i zestaw wzorców służących do budowania niezawodnych, współbieżnych i łatwych w utrzymaniu systemów w Erlangu.

Najważniejsze idee:

- Erlang ma bardzo lekkie i potężne procesy.
- Ale jeśli za każdym razem ręcznie piszemy:
  - `spawn`, `spawn_link`
  - monitory oraz linki
  - pętle `receive`
  - obsługę błędów i zakończenia procesu
  – łatwo popełnić błąd.

**OTP** dostarcza gotowych struktur (behaviours), które ukrywają całą tę powtarzalną logikę.  
Najważniejszym z nich dla serwerów jest **`gen_server`**.

W tej prezentacji:

1. zobaczymy naiwny ręczny serwer,
2. wyodrębnimy część wspólną do modułu `my_server`,
3. zrozumiemy, skąd bierze się idea stojąca za `gen_server`.

---

## Wzorzec „podstawowego serwera”

Większość serwerów w Erlangu wygląda podobnie:

1. **start/init** – uruchomienie procesu i ustawienie stanu początkowego,
2. **loop(State)** – nieskończona pętla `receive`:
   - odbieranie wiadomości,
   - aktualizacja stanu,
   - powrót do `loop(NewState)`,
3. dodatkowa logika: monitory, timeouty, obsługa zakończenia itd.

To schemat, który powtarza się w każdym serwerze — więc można go zautomatyzować.

---

## Krok 1. Naiwny `kitty_server`

Zbudujmy prosty sklep z kotami:

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
