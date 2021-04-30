Erlang по-большому
==================

Modules
-------
    compile:file(my_module)

Где ищется модуль
* cd
* `$OPTROOT/lib/*`
    - use 'ebin' if present
    - if version number is present,
      choose the most recent one.
* Все подкаталоги каталогов, `$ERL_LIBS`
    - if no 'ebin' is present

Берём первый встретившийся модуль с совпадающим
именем.

Отдельные виртуальные машины называются нодами.
По-умолчанию используется TCP/IP.

Предположительно, весь код на всех нодах находится
под нашим контролем.

Ноды в кластере должны иметь имена.

-name -- full name

    net_kernel:connectnode(first_node@debian).
        transitivity.

Можно создать процесс на другой ноде и даже её не спросить.

    NewPid = spawn(
        third_node@debian,
        fun () ->
            receive
                {ping, P} ->
                   P ! pong
            end
        end
    ).
