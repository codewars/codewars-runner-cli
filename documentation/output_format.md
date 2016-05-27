A custom and very basic format is used for sending data out of the CLI tool. All formatted data is returned via STDOUT. 
If you do nothing but write normal strings to STDOUT, then codewars.com will display each line as you would expect, unformatted.

A small subset of commands is supported that can be used to format output. They are:

- `<DESCRIBE::>`
- `<IT::>`
- `<PASSED::>`
- `<FAILED::>`
- `<ERROR::>`
- `<COMPLETEDIN::>`

Prefixing a new line with these commands will cause that line to be formatted. 
Since each new STDOUT line is considered a new peace of data, if you wish to format multiple lines as one 
item (such as a multi line "passed" message), then you must replace all \n line feed characters with the `<:LF:>` token.

For example, in Ruby, if you wanted to write a multi-line passed message:

```ruby
def passed(msg)
  puts "<PASSED::>#{msg.gsub("\n", "<:LF:>")}"
end
```

### Nested Describes

Some test frameworks support nested levels of describes. In order for our output to support multiple levels, 
you must also use the `<COMPLETEDIN::>` token, which acts as a terminator for the current item. This should be used 
to terminate both `<DESCRIBE::>` and `<IT::>` statements.

The following is a full example of what the output might look like, that supports nested describes:

```
<DESCRIBE::>Foo
<IT::>It should return a string
<PASSED::>Test Passed
<COMPLETEDIN::>23
<IT::>It should return "foo" 
This is some direct output (i.e. console.log("..."))
<FAILED::>Expected "foo" but instead got ""
<COMPLETEDIN::>10
<DESCRIBE::>This is a nested describe
<IT::>Should not be null
<PASSED::>Test Passed
<COMPLETEDIN::>20
<COMPLETEDIN::>22
<COMPLETEDIN::>100
```

> Notice how there are 3 `<COMPLETEDIN::>` statements at the end. The first one completes the last IT
statement, the 2nd completes the nested DESCRIBE and the 3rd completes the top level "Foo" DESCRIBE.

#### <COMPLETEDIN::> Details

The value of COMPLETEDIN should be time spent executing the related statement, in milliseconds. It is not required
to support time. `<COMPLETEDIN::>` is valid on its own, and in that case it is only used to terminate the current statement. 

### Why the custom format?

Getting different test suites in different languages to all play together with the same format can be tricky. In many cases, 
customizing the test suite output is very limited (sometimes requiring hacking). Because of this, using formats such as 
XML and JSON are complicated because its not always possibly to correctly close out the data format when a program raises an exception. 

The format choosen was originally done so that at any point in time the program could exit while still having readable data.
Other formats, such as TAP (Test Anything Protocol) could also be an option. However another requirement that we had when
designing the format was to have it be incredibly simple yet flexible, so that Codewars.com could support more than simply
outputing test results. With the current format there is nothing stopping you from outputing HTML, JS canvas code, etc in order
to create a rich and even interactive test result output.