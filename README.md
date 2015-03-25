Small program to render random texts from XML-file. See `examples/` for example usage.

Installation
------------

Compile on your own. Made in OCaml. See Makefile or message me and I will extend this section.

On Ubuntu:

* Clone repository
* Install OCaml like `sudo apt-get install ocaml ocaml-findlib opam`
* Opam needs some configuration, look it up
* Use opam to install xml-light and pcre
* Print `make` in dir
* Use `./tellstory filename.xml` to render story.

Tags
----

Tags with special meaning.

#### Story

Outermost tag. Mandatory.

#### Sentence and alt

A sentence will choose from the different alternatives:

    <sentence>
      This is a
      <alt>sentence.</alt>
      <alt>joke.</alt>
    </sentence>

It's possible to have an empty sentence and just `<alt>`s, like this:

    <story>
      <sentence>
        <alt>Some alternative.</alt>
        <alt>Something else entirely.</alt>
      </sentence>
    </story>

One alt is chosen randomly. All alts have the same probability.

#### Linebreaks

Use `<br />` to insert a line break.

#### Comments

Make comments in the like in any HTML or XMl document, with `<!-- bla bla -->`.

#### Flags

When an alternative is chosen, you can set a flag to be used later:

    <sentence>
      This is a
      <alt setFlag="flag1">sentence.</alt>
      <alt>joke.</alt>
    </sentence>

A flag can _only_ be set once! The following story will throw an exception:

    <story>
      <sentence>
        This is a 
        <alt setFlag="flag1">sentence.</alt>
      </sentence>
      <sentence>
        This is another 
        <alt setFlag="flag1">sentence.</alt>
      </sentence>
    </story>

    Problem with sentence 'This is another'
    Fatal error: exception Main.Flag_already_set("flag1")

A flag is used to conditionally print a sentence:

    <story>
      <sentence>
        This is a 
        <alt setFlag="flag1">sentence.</alt>
        <alt>joke.</alt>
      </sentence>
      <sentence ifFlagIsSet="flag1">
        This is only print if flag1 is set.
      </sentence>
    </story>

One can also use `ifSet` as a short-hand alternative to `ifFlagIsSet`.

You can use `ifSet` in the `<alt>` tag to limit possible alts to be choosen in the sentence.

Example: todo

#### Macros

Name must be unique. Define a macro with tag `<macro name="macroName">`. The macro can then be used on many places instead of alt:s. Use a macro with `<alt useMacro="macroName"></alt>`. The macro is randomized anew each time it's used.

Example:

    <story>

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

    </story>

#### Variables

Variables is a way to store `<alt>`s to be used again, without randomization. Variables can be parsed in sentences and alts with these curly braces: `{}`. One use-case for this is genders or names, which might be randomized once but used several times.

Variables can use letter a-z, A-Z, 0-9 and '\_'.

Example:

#### Records

Records are much like variables, just a tad more complex: You can save many different data fields in one "go", or randomization. This is good for gender pronouns like "he, him, his" vs "she, her, hers". In this way, you get all pronouns kept at one and same place.

Example:

#### Inline macros

Able to use macros direct in sentence like {#macro}. So instead of

    <sentence>
      A sentence with just one macro...
      <alt useMacro="my_macro"></alt>
    </sentence>

you can write

    <sentence>A sentence with just one macro... {#my_macro}</sentence>

#### Include

`<include>` tag to include other files in your story.

#### Advanced use-cases

* Using macros in variables

Possible future features
------------------------

* Deck, where each alt is chosen atleast once during execution
* Inline decks, like {$deck1}?
* Many stories in one file, choose one randomly.
* Logical operators in `ifSet`, like `ifSet="(flag1 AND flag2) OR flag3"`.
* Debug information saved in text file
* Export to PDF or markdown.
* Web interface
* Use case where we want to generate many characters, all with different names and genders, where no name is used more than once.
