# COBOL Copybook parser in SCALA

This is a COBOL Copybook parser in scala. This parses all fields from given copy book and creates an output file with field,data type, signed/unsigned,packed/binary,length. While doing so, it also handles Occurs and Redefines in a specific way. Tested with example.cbl file in this repo. This will help to further parse or format the copybook.

### How to run
Below is a sample to run
```
scala CopyBookParser.scala ./example.cbl ./example_out.txt
```

Please check the example.cbl and example_out.txt.

