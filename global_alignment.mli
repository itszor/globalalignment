val needleman_wunsch : ?config:'a Config.params -> 'a array -> 'a array ->
                       ('a option * 'a option) list

val hirschberg : ?config:'a Config.params -> 'a array -> 'a array ->
                 ('a option * 'a option) list
