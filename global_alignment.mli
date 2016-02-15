module Config : sig
  type 'a params =
    {
      ins : int;
      del : int;
      sub : 'a -> 'a -> int
    }

  val default : 'a params
end

val needleman_wunsch : ?config:'a Config.params -> 'a array -> 'a array ->
                       ('a option * 'a option) list

val hirschberg : ?config:'a Config.params -> 'a array -> 'a array ->
                 ('a option * 'a option) list
