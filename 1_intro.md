
Введение в Erlang
=================

Типы данных в Erlang.
---------------------

* Целые (integer), вещественные (float), строки (string)
* Atoms (atom): `foo`, `b_a_r`, 'BAZ' (если с большой)
* Гетерогенные списки: [1, 2, 3.0, "four", [5,6]]
* Кортежи: {1, 2, [3, {4, 5}]}
* Функции: `fun () -> ok end`
* PIDs

Строка - список целых чисел
$A - код буквы 'A'
"foo" == [$f, $o, $o]

Операторы
---------

Как обычно, за исключением:
/= - неравенство;
=> - больше либо равно;

Целое и вещественное могут быть равны друг другу.
Лучше использовать точное равенство: =:=

Арифметика.
Обычные операторы.
Инфиксные операторы (лол): div, rem

Логические операторы: and, or, xor, not
Ленивые логические операторы: andalso, orelse

Действия со списками: ++, -- (не используйте его, дети)
Конструирование списков: `[Head1, Head2, Head3, | Tail]`

List comprehensions. [ {A, B} || A <- kokok, B <- kokoko ]

Сопоставление с образцом.

{ A, _ } = {1, 2}
[ B, C | T ] = [3,4,5,6]
D = {{foo1, foo2}, bar}
{{foo1, E}, F} = D

case
используется точное сравнение

Если переменная уже связана, то в case она будет
участвовать в сопоставлении с образцом, а не
присваиваться.

Erlang - ЛУЧШИЙ OOP, ну да.


Порождение процессов
--------------------

    Pid = spawn(F)
    Pid ! Message
    receive ... end

We can do this:
    spawn(fun other_process/0),
    io:format("Process spawned").

Сообщения от одного процесса приходят по порядку.

Есть TCO

Если сообщение не матчится, оно остаётся в мэйлбоксе.
