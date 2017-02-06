# Setup Configuration Statements

Every language supports a `setup` property, which allows you to configure code that should be ran before the main code block.
The `setup` property can also be used to configure the environment in which the code will run, through the use of special
configuration statements. 

The setup code block is usually executed in the language of the main code block (except for SQL and Shell, in which Ruby is used).
For this reason, you will define configuration statements within a comment for the specific language that is being targeted. 

For example, if you were executing JavaScript and wanted to pull down the contents of a Github repo into the working directory
before the JavaScript code is ran, you could provide the following setup code:
 
```javascript
// @download-github-repo someuser/somerepo
```

Here it is defined for Ruby:
```ruby
# @download-github-repo someuser/somerepo
```

## Supported Configuration Statements

### `@download-github-repo`

The entire contents of a github repository will be pulled into the same working directory as the code that is being executed.

A few things to note:

- You should keep the download as light as possible, it adds load time to the request
- If you want to download a specific branch, you can do it using this format `:username/:reponame/tarball/:branchname`
- Targetting specific folders of a repo is not supported. If you want to share a single repo for use with multiple languages, use unique branches instead.


### `@run-shell-script`

This statement is useful when combined with the download-github-repo statement. It specifies a shell script within the 
download that should be executed before the code is ran. This script is given its own 10 second timeout which doesn't 
apply to the main code execution timeout.

**Example:**

```
@run-shell-script start.sh
```

### `@use-database` (SQL only)

This statement is available when running SQL. Currently its usage is further limited to being used only for PostgreSQL.
It determines a existing database already on the image that should be used. By default, the only database available on 
the image is `dvdrental`, which points to this [sample database](http://www.postgresqltutorial.com/postgresql-sample-database/).
 
**Example:**

```
@use-database dvdrental
```
