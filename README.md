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

    <sentence>
      This person here is a
        <alt setFlag="man">real man.</alt>
        <alt setFlag="woman">real woman.</alt>
    </sentence>

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

    he took his things and left.

or

    she took her things and left.

The `<alt>`:s in the record must have exactly the same inner structure, in this case `<he>` and `<his>`. Then you use the inline dot-notation `{like.this}` to access the records fields.

#### Inline macros

Instead of using `<alt useMacro="macro_name">`, you can use the short-hand inline `{#macro_name}`:

    <sentence>A sentence with just one macro: {#my_macro}</sentence>

#### Include

`<include>` tag to include other files in your story.

#### Advanced use-cases

* Using macros in variables

Possible future features
------------------------

* Deck, where each alt is chosen atleast once during execution, but never twice or more. Use-case: Names.
* Inline decks, like {$deck1}?
* Many stories in one file, choose one randomly.
* Logical operators in `ifSet`, like `ifSet="(flag1 AND flag2) OR NOT flag3"`.
* `ifNotSet`
* Debug information saved in text file
* Export to PDF or markdown.
* Web interface
* Use case where we want to generate many characters, all with different names and genders, where no name is used more than once.
* Inline randomization: `{this_variable|or_this|#or_this_macro}`.

TODO
----

* Unit tests
