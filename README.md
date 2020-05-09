Small program to render random texts from XML-file. See `examples/` for example usage.

Can be tried out on this home page: http://tonesoftales.com/tellstory

Installation
------------

Compile on your own. Made in OCaml. See Makefile or message me and I will extend this section.

On Ubuntu:

* Clone repository
* Install OCaml like `sudo apt-get install ocaml ocaml-findlib opam`
* Opam needs some configuration, look it up
* Use opam to install xml-light, pcre, menhir, core, bolt
* Print `make` in dir
* Use `./tellstory filename.xml` to render story.

Tags
----

Tags with special meaning.

#### Story

Outermost tag. Mandatory.

    <story>
      <!-- Content here -->
    </story>

Some configuration goes into the `<story>` tag. Right now, this is supported:

* namespace - name of namespace for this story (used when including files)
* newline="print" - when set, adds a newline to every print or sentence tag to avoid writing `<br/>`

#### Sentence

The sentence is the fundamental building block. A simple sentence will be printed as is.

    <sentence>A simple sentence.</sentence>

Then run, from the shell, `$./tellstory filename.xml`, and the program will print

> A simple sentence.

You can also use the tag `<print>` - it works exactly the same as `<sentence>`.

#### Alt

Within a sentence you can have `<alt>` tags (short for _alternative_). One alt tag will be chosen randomly:

    <sentence>
      This is a
        <alt>sentence.</alt>
        <alt>joke.</alt>
    </sentence>

It's possible to have an empty sentence and just `<alt>`s, like this:

    <sentence>
      <alt>Some alternative.</alt>
      <alt>Something else entirely.</alt>
    </sentence>

All alts have the same probability.

#### Linebreaks

Use `<br />` to insert a line break.

#### Comments

Make comments in the like in any HTML or XMl document, with `<!-- Bla bla -->`.

#### Flags

When an alternative is chosen, you can set a flag to be used later:

    <sentence>
      This person here is a
        <alt setFlag="man">real man.</alt>
        <alt setFlag="woman">real woman.</alt>
    </sentence>

A flag can _only_ be set once!

A flag is used to conditionally print a sentence:

    <sentence ifSet="man">
      Yes, he is indeed a man.
    </sentence>

You can use `ifSet` in the `<alt>` tag to limit possible alts to be choosen in the sentence.

    <sentence>
      Hello, my
        <alt ifSet="woman">old lady!</alt>
        <alt ifSet="woman">young lady!</alt>
        <alt ifSet="man">man!</alt>
    </sentence>

If no alt is possible to choose, an error will be printed.

##### More flag checking with AND, OR and NOT

You can use keywords `AND`, `OR`, and `NOT` when checking which flags are set.

Examples:

* To check that two flags are set, use `ifSet="flag1 AND flag2"`
* To check that either one of two flags are set, use `ifSet="flag2 OR flag2"`
* To check that one flag is _not_ set, use `ifSet="NOT flag2"`

These can be combined with paranthesis:

* To check that two flags are _not_ set, use `ifSet="NOT (flag1 AND flag2)"`

And so on.

#### Macros

A macro is a bunch of `<alt>`:s that can be re-used in many different places, each time with a new randomization. The macro name must be unique. Define a macro with tag `<macro name="macroName">`.  Use a macro with `<alt useMacro="macroName"></alt>`.

Example:

    <macro name="material">
      <alt>stone</alt>
      <alt>wood</alt>
      <alt>bronce</alt>
      <alt>copper</alt>
    </macro>

    <sentence>
      This thing is made out of
        <alt useMacro="material"></alt>   <!-- Random alt -->
    </sentence>

    <sentence>
      This other thing is made of
        <alt useMacro="material"></alt>   <!-- New randomization here -->
    </sentence>

#### Variables

Variables is a way to store `<alt>`:s to be used again, without randomization. Variables can be parsed in sentences and alts with these curly braces: `{variable_name}`. One use-case for this is genders or names, which might be randomized once but used several times.

Variable names can use letter a-z, A-Z, 0-9 and '\_'.

Example:

    <variable name="name">
      <alt>John</alt>
      <alt>Joe</alt>
      <alt>Jim</alt>
    </variable>

    <sentence>
      This person here is called {name}.
    </sentence>

If you don't want to randomize the variable value, you can set it directly with the "value" attribute:

    <variable name="name" value="John" />

#### Records

Records are much like variables, just a tad more complex: You can save many different data fields in one "go", or randomization. This is good for gender pronouns like "he, him, his" vs "she, her, hers". In this way, you get all pronouns kept at one and same place.

Example:

    <record name="gender">
      <alt>
        <he>he</he>
        <his>his</his>
      </alt>
      <alt>
        <he>she</he>
        <his>her</his>
      </alt>
    </record>

    <sentence>
      {gender.he} took {gender.his} things and left.
    </sentence>

This will print either

> he took his things and left.

or

> she took her things and left.

The `<alt>`:s in the record must have exactly the same inner structure, in this case `<he>` and `<his>`. Then you use the inline dot-notation `{like.this}` to access the records fields.

TODO: Use record without any `<alt>`, e.g. for player and monster data (together with `<list>` etc).

#### Decks

Deck is a way to randomize alternatives without any single alternative being chosen twice, just like a deck of cards. Overly romantic example:

> Olle loves Birte

    <deck name="names">
      <alt>Olle</alt>
      <alt>Birte</alt>
    </deck>

    <sentence>
      <alt useDeck="names">
    </sentence>

    <sentence>
      loves
        <alt useDeck="names">
    </sentence>

When all cards have been "picked", the deck can't be used again unless you set attribute shuffle="true" to automatically shuffle it.

    <deck name="adeck" shuffle="true">
      <alt>One</alt>
      <alt>Two</alt>
      <alt>Three</alt>
    </deck>
    <!-- Pick all cards from deck three times -->
    <loop times="9">
      <sentence>{$adeck}</sentence>
      <br/>
    </loop>

TODO: Rename alt to card.

#### Dice

To roll a dice, you need to first define its name and number of sides, like so:

    <dice name="d6" sides="6" />

You roll it using the inline construct `%`:

    <sentence>Roll the dice! You get {%d6}.</sentence>

You can roll one dice multiple times on one use by adding a dot and a number:

    <sentence>Roll the dice twice! You get {%d6.2}.</sentence>

#### Inlining alternatives, macros, variables, records and deck

Instead of using `<alt useMacro="macro_name">`, you can use the short-hand inline `{#macro_name}`:

    <sentence>A sentence with just one macro: {#my_macro}</sentence>

The same goes for content, records and variables. Inline alternatives is like inline `<alt>` tags, where the content is within quotes:

    <sentence>A sentence with just two inline alternatives: {"this"|"that"}.</sentence>

For variables and records:

    <sentence>A sentence with just one variable: {var}</sentence>

    <sentence>A sentence with just one record: {das.ding}</sentence>

For decks we use the character `$`, like this:

    <sentence>Pick a card, any card: {$deck}.</sentence>

For dice we use the character `%`:

    <sentence>Roll the dice! You get {%my_dice}.</sentence>

#### Inline randomization

Choose between variable, record or macro directly in sentence content without writing `<alt>`:s, using barline `|`.

Example where one of four alternatives will be chosen:

    <sentence>Could be any of this: {#macro1|$deck|record.something|variable}.</sentence>

#### Include

Include another file in you script.

    <include file="example/myexample.xml" />

All macros, records and so on will be present after the inclusion. So you can't include the same file twice which define the same variable/record/macro.

It's also possible to include files using randomization:

    <sentence>
      What should we print today?
        <alt include="file1" />
        <alt include="file2" />
    </sentence>o

or in a deck or macro:

    <deck name="mydeck">
      <alt include="file1.xml" />
      <alt include="file2.xml" />
    </deck>

Be careful of how you name things in different files, since you can get name collisions if two files define the same deck or macro name.

#### More about flags

You can set a flag without using `<sentence>` or `<alt>` tags like this:

    <setFlag name="flag1" />

This is useful for when a file is included randomly. E.g., if the file `dragon.xml` is included, you might want to check in other files for this condition.

In the same way you can use `ifSet` as its own tag:

    <ifSet name="flag1">

      <macro name="flag1_macro">  <!-- Macro only defined if flag1 is set!
        <alt>Hi</alt>
        <alt>Ho</alt>
      </macro>

      <!-- All other tags can be used too ... -->

    </ifSet>

The flag mini-language is available as expected, so you can print `(flag1 AND flag2) OR NOT flag3` etc in flag condition.

#### Clearing variables, macros and flags

Sometimes you want to include a file more than once in your setup. In such a case, it can be useful to clear everything that was used so it can be redefined and generated anew. To do this, use the `<clear>` tag.

    <clear type="macro" name="myMacro" ignore-not-found="1" />

If the macro has not be defined before, the clear command will abort the execution. To ignore such errors, use the `ignore-not-found` attribute as given above.

Other examples:

    <clear type="variable" name="myVariable" ignore-not-found="1" />

    <clear type="flag" name="theDragonHasCome" ignore-not-found="1" />

#### Namespaces

When you include many different files from within another file, names of variables and macros might clash. To solve this, we use namespaces, a common concept from programming languages. Each file creates its own namespace. Can also create new namespaces just by naming them, like `<variable name="asd" namespace="new_namespace">asd</variable>`.

The namespace "global" is used by default.

To add a variable to a specific namespace (and not to global), write this:

    <variable name="var" namespace="my_namespace">
      <alt>This variable is in namespace "my_namespace".</alt>
    </variable>

The same attribute `namespace` can be used when declaring macros, decks, records etc.

To use a specific namespace in a sentence:

    <sentence namespace="my_namespace">{var}?</sentence>

You can also specify which namespace to use inline:

    <sentence>{my_namespace\var}?</sentence>

#### Loops

A loop lets you repeat a sentence or any number of sentences a number of times. The number can be random.

Example:

    <loop times="5">
        <sentence>Print this five times</sentence>
        <br/>
    </loop>

Example using a random number of times:

    <loop rand="3">
        <sentence>Print this one, two or three times</sentence>
        <br/>
    </loop>

A loop can run until a flag is set. To do this, use the attribute "until" like this:

    <loop until="endloop">
        <sentence>
            <alt>Some</alt>
            <alt>alts</alt>
            <alt>to</alt>
            <alt>wait</alt>
            <alt>for</alt>
            <alt setFlag="endloop">flag</alt>
        </sentence>
    </loop>

The word "flag" will always be the last word in the above loop.

A loop which waits for a flag will loop maximum 999 times.

#### Graphs

In a graph, you have nodes that connect to other nodes. When picking a new node randomly, you only pick a node that's _connected_ to the present node.

    <graph name="my_graph">
      <node id="1" connections="2,3">Center node</node>
      <node id="2" connections="1">Other content</node>
      <node id="3" connections="1">Third node content</node>
    </graph>

    <loop times="10">
        <sentence>{@my_graph}</sentence>
        <br/>
    </loop>

You can assign which node to start on with the "start" attribute, like so:

    <graph name="graph_with_start" start="99">
        <node id="99" connections="1">Never printed</node>
        <node id="1" connections="2">This is always printed first</node>
        <node id="2" connections="1">Second, then back again</node>
    </graph>

A node can point to itself as a connection. You can also repeat the same number to manipulate probability:

    <graph name="prob_test">
      <node id="1" connections="1,1,1,1,2">Happens often</node>
      <node id="2" connections="1">Happens rarely and only one at a time</node>
    </graph>

TODO: Graph nodes can set flags, e.g. to exit a loop.

TODO: Nodes with `<input>` in node to choose connection in graph.

TODO: connections can be inline expression

#### WIP

Currently work-in-progress.

    <sleep/>
    <sleep time="1"/>
    <input name="name" label="What's your name, traveller? "/>
    <input name="path" label="Choose your path (1-4): " validation="[1-4]" />
    <if variable="name" equals="asd">...</if>
    <if ...><then></then><else></else></if>
    <if variable="var" higherThan="12">...</if> (* Only for integers *)
    <if variable="var" lessThan="12">...</if> (* Only for integers *)
    <if content="{eval this}" equals="12">...</if>, also for higherThan and lessThan
    <sentence>{:inlineinput}</sentence>
    <node><input name="name" input="What's your name? "/></node>
    <set variable="var1" value="foo" />
    <set record="player" field="hitpoints" value="{player.hitpoints - 1}" />
    <list><record ...></list>
    <add to="list" name="listName">...</add>
    TODO: <remove from="list" name="asd" item="id"...
    TODO: add/remove to deck, macro, graph
    +, -
    TODO: *, /, floats
    TODO: <print list="name" field="fieldname"/>
    TODO: <assert variable="" /> ? list_item?
    TODO: <loop graph="asd" until="" /> untilNode? untilFlag? untilnode, untilflag
    TODO: <pick from="list" name="players" record="player" />
    TODO: Equipment

Possible future features
------------------------

* Unset flag?
* Many stories in one file, choose one randomly.
* Export to PDF or markdown.
* Web interface
* Community where you an share story snippets and use each other's stories.
* Use-case where we want to generate many characters, all with different names and genders, where no name is used more than once.
* Possibility to use JSON format instead of XML.
* GUI to open XML-file and see it printed, intead of command-line interface? Web page instead.
* Markov chains...?
* Functions (filters) inline to change e.g. upper/lower case {uppercase(variable)}. Or bold/cursive? But can just as well do *{somethinginbold}*?
* Random numbers... Int, float? Char? "Pick random number of cards."
* Normal distribution (useful for age, weight, etc)
* Nesting namespaces
* Simplify declaration of sing/plur records. How?
* All flags are global?
* Graphs with random start number
* Multiplayer
* Graph combat example
* Monopoly example - requires state? Move card from deck to player's bag or slot, card needs id etc; slot? brick? Move card from deck to hand? Might as well use Tones of Tales.
  `<player>`, `<nextPlayer variable="currentplayer"/>`, `<slot>`, `<brick>` that can be moved to slots; cards that can apply to active player. Player's turn is like a graph?
* Need to loop a list? players, monsters, npc, decks?
* `<monster>` tag for combat?
* `<npc>` tag for trade, dialog?
* `<dialog>` tag for easy dialog graphs?
* `<lobby>` for remote play
* Non-turn based play?
* No clear separation between tags that declare something and tags to do something?

TODO
----

* Don't throw sentence exception everywhere... Confusing. Only for `<sentence>` tag.

Use-cases
---------

* Timeline. Each day something is happening. "Day 1: ... Day 2: ..."
* Generate table for treasure, meetings, ...
* Generate character
