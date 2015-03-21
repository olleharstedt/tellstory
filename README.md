Small program to render random texts from XML-file. See main.xml for example usage. More features planned.

Sentence and alt
----------------

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

Linebreaks
----------

Use `<br />` to insert a line break.

Flags
-----

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

Macros
------

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

Variables
---------

Variables is a way to store `<alt>`s to be used again, without randomization. Variables can be parsed in sentences and alts with these curly braces: `{}`. One use-case for this is genders or names, which might be randomized once but used several times.

Example:

Records
-------

Records are much like variables, just a tad more complex: You can save many different data fields in one "go", or randomization. This is good for gender pronouns like "he, him, his" vs "she, her, hers". In this way, you get all pronouns kept at one and same place.

Example:


