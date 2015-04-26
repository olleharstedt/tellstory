Small program to render random texts from XML-file. See `examples/` for example usage.

Installation
------------

Compile on your own. Made in OCaml. See Makefile or message me and I will extend this section.

On Ubuntu:

* Clone repository
* Install OCaml like `sudo apt-get install ocaml ocaml-findlib opam`
* Opam needs some configuration, look it up
* Use opam to install xml-light, pcre, menhir
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

#### Sentence

The sentence is the fundamental building block. A simple sentence will be printed as is.

    <sentence>A simple sentence.</sentence>

Then run, from the shell, `$./tellstory filename.xml`, and the program will print

> A simple sentence.

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

When all cards have been "picked", the deck can't be used again. (Possible TODO: Add support to re-shuffle deck.)

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

#### Inline randomization

Choose between variable, record or macro directly in sentence content without writing `<alt>`:s, using barline `|`.

Example where one of three alternatives will be chosen:

    <sentence>Could be any of this: {#macro1|record.something|variable}.</sentence>


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

Possible future features
------------------------

* Shuffle deck when cards are empty? `<deck name="deck1" shuffle="true">`.
* Unset flag?
* Many stories in one file, choose one randomly.
* `ifNotSet`? Possible now with `NOT flag1`.
* Debug information saved in text file
* Export to PDF or markdown.
* Web interface
* Use-case where we want to generate many characters, all with different names and genders, where no name is used more than once.
* Possibility to use JSON format instead of XML.
* GUI to open XML-file and see it printed, intead of command-line interface? Web page instead.
* Markov chains...?
* Include in `<alt>` to randomly include a file
* `<ifSet name="...">` to print sentences only if flag is set.

TODO
----

* Proper regexp for inlining, e.g. this should not parse: {"content"|variable with spaces}
* Better error messages at namespace conflicts when including different files
