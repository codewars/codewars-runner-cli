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
Since each new STDOUT line is considered a new piece of data, if you wish to format multiple lines as one 
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
 
### Advanced Formatting

So far the basic formatting needed to be supported by a language has been discussed. The following
details more advanced features.

#### Advanced Format Schema
The basic `<[TAG]::>[VALUE]` tag format described previously can be expanded to support modes and labels. 
This is done using the following convention: `<[TAG]:[MODE]:[LABEL]>[VALUE]`. Modes and labels are optional.

For example. To write a log message with a custom container label. You can do this: `<LOG::My Label>My log message`.
In this example, we provided a tag (required) and a label, and skipped using a mode. 
 
#### What are modes?
A mode is used to output richer content. For example there is a TABLE mode which parses the value as JSON and renders a 
table out of it.

#### Advanced Tags

##### LOG
Normally each unformatted line is considered a "LOG" message and is written to STDOUT. Visually this
output will be presented withint a Log container. Multiple log lines are grouped together automatically.

In order to access richer log output content (via modes), you will need to explicitely use a LOG tag. 

##### OUT
Same as LOG, however it does not wrap the output within a container UI.

##### TAB
Should be used directly after a LOG tag. Will add a 2nd tab to the log container. Otherwise treated the
same as a LOG tag. You can add multiple tabs to a LOG container.

```
<LOG::Tab 1>This is content displayed within the first tab.<:LF:>This is a 2nd line
<TAB::Tab 2>This is content displayed within the 2nd tab.
```

#### Advanced Modes

##### TABLE
##### CHART
##### DIFF
##### MARKDOWN
##### JSON
##### HIGHLIGHT
##### SCRIPT

In this mode, the tag VALUE will be considered a URL and a script will automatically be
loaded on the page. A specific file will only be loaded once.

```
<LOG:SCRIPT:>https://raw.githubusercontent.com/name/repo/file.js
```


### Why the custom format?

Getting different test suites in different languages to all play together with the same format can be tricky. In many cases, 
customizing the test suite output is very limited (sometimes requiring hacking). Because of this, using formats such as 
XML and JSON are complicated because its not always possibly to correctly close out the data format when a program raises an exception. 

The format choosen was originally done so that at any point in time the program could exit while still having readable data.
Other formats, such as TAP (Test Anything Protocol) could also be an option. However another requirement that we had when
designing the format was to have it be incredibly simple yet flexible, so that Codewars.com could support more than simply
outputing test results. With the current format there is nothing stopping you from outputing HTML, JS canvas code, etc in order
to create a rich and even interactive test result output.
