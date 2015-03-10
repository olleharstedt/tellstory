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

A flag can _only_ be set once!
