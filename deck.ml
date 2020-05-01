(**
 * Module for the <deck> tag
 *)

module Make (Common : Common.S) (Alt: Alt.S) = struct
  exception Deck_exception of string

  (** Data types for storing decks *)
  type t = {
    name : string;
    alts : Alt.t list;  (* When an alt is chosen it's removed from alts and put in trash. *)
    trash : Alt.t list;
    shuffle_on_empty : bool;
  }

  type tbl = (string, t) Hashtbl.t

  (**
   * Choose card from deck and remove it from the deck.
   * Return card content.
   *
   * @param deck_name string
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  let rec eval_deck deck_name (state : Common.state) (namespace : Common.namespace) : string =
    Common.log_trace "eval_deck\n";
    let deck = try Hashtbl.find namespace.deck_tbl deck_name with
      | Not_found -> raise (Deck_exception (sprintf "Found no deck with name '%s'." deck_name))
    in

    (* Abort or shuffle if no cards are left in deck *)
    let deck = begin match List.length deck.alts, deck.shuffle_on_empty with
      | 0, true -> {deck with alts = deck.trash; trash = []}
      | 0, false -> raise (Deck_exception (sprintf "No more cards in deck '%s'." deck_name));
      | _, _ -> deck
    end in
    let cards = get_possible_alts deck.alts in
    let pick_nr = Dice.dice (List.length cards) in
    let card = List.nth cards pick_nr in

    match card with
    | exception Not_found -> raise (Deck_exception ("Found no card"))
    | card ->
        (* Create a new deck without the card we picked *)
        (* Card = alt in this context *)
        (* Filter logic:
          * We want to filter the card we just picked.
         * All cards that are not identical to this card should return false (not filter)
         * The card is identical to the card we picked IF:
           *   content = content AND attributes are equal (name && content of attribues are equal)
         *)
        (** TODO: Start from 0 or 1? Previously used Core, but won't compile on ARM. *)
    let i = ref (-1) in
    let new_alts = List.filter (fun alt ->
      i := !i + 1;
          !i != pick_nr
    ) deck.alts in
    let new_deck = {deck with alts = new_alts; trash = card :: deck.trash} in
    log_trace "old_deck: ";
        log_trace "new deck: ";
        print_deck new_deck;
        Hashtbl.remove namespace.deck_tbl deck.name;
        Hashtbl.add namespace.deck_tbl new_deck.name new_deck;
        (* Build Xml.xml <alt> out of record *)
        let alt = Xml.Element ("alt", card.attributes, [Xml.PCData card.content]) in
        let result : string = eval_alt alt state namespace in
        log_trace (sprintf "eval_deck: result of eval_alt = %s\n" result);
        result
end
