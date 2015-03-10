Small program to render random texts from XML-file. See main.xml for example usage. More features planned.

Features:

A sentence will choose from the different alternatives:

    <sentence>
      This is a
      <alt> sentence.</alt>
      <alt> joke.</alt>
    </sentence>

When an alternative is chosen, you can set a flag to be used later:

    <sentence>
      This is a
      <alt setFlag="flag1"> sentence.</alt>
      <alt> joke.</alt>
    </sentence>

A flag can _only_ be set once! The following story will throw an exception:

    <story>
      <sentence>
        This is a 
        <alt setFlag="flag1"> sentence.</alt>
      </sentence>
      <sentence>
        This is another 
        <alt setFlag="flag1"> sentence.</alt>
      </sentence>
    </story>

    Problem with sentence 'This is another'
    Fatal error: exception Main.Flag_already_set("flag1")

A flag is used to conditionally print a sentence:

    <story>
      <sentence>
        This is a 
        <alt setFlag="flag1"> sentence.</alt>
        <alt> joke.</alt>
      </sentence>
      <sentence ifFlagIsSet="flag1">
        This is another 
        <alt> sentence.</alt>
      </sentence>
    </story>
