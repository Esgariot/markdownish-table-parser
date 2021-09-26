# markdownish-table-parser
Parsing markdown-like table into tokens, and then parsing the tokens into specific domain

This code aims to parse something like this

```markdown
| Component       | 100     | 100             | 101         | 100     | 101     |
| ColumnName      | a       | b               | Parent      | Id      | Id      |
| ColumnType      | int     | string          | parent      | id      | id      |
| --------------- | ------- | --------------- | ----------- | ------- | ------- |
|                 | 10      | abba            |             | 1..10   |         |
|                 |         |                 | 1           |         | 2       |
```

into domain specific types. 

Currently it parses table structured like one above into 2-dimensional array of string tokens.
It has some parsers that know how to deal with specific cell contents, but there are no abstractions
that run the parsers on list of tokens and combine the results. 
