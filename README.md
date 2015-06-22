# Overview

Trurl is a haskell scaffolding tool named after great hyper-space engineer-constructor Trurl who (with his friend Klapaucius) is the hero of Stanislaw Lem's fiction.

![Trurl](https://raw.githubusercontent.com/dbushenko/trurl/master/img/trurl.jpg "Trurl")

The tool is able to create template projects of any type and template files of any type. Though it was intended to generate template haskell code, you may also generate html/css/js in web projects and any other text files.

## Installation

Build it from source using cabal:

    git clone https://github.com/dbushenko/trurl.git
    cd trurl
    cabal sandbox init
    cabal install --dependencies-only
    cabal build
    cabal install

Or install it from the repository:

    cabal update
    cabal install trurl-0.3.1.0

## Quick Start

Run following comments to create a scotty/mysql project:

    trurl create MyProject scotty-mysql

Go to MyProject/src, run following command to generate entity Comment:

    trurl new Comment scotty-entity 'props:[comment#String, author#String@]'

## Usage

Just run 'trurl' to see the help:

    trurl <command> [parameters]
      update -- fetch the updates from repository
      create <name> <project_template> -j [parameters_string] -- create project of specified type with specified name; optionally add JSON parameters, wrap it with \"\" or ''
      create <name> <project_template> [parameters] -- create project of specified type with specified name; optionally add parameters
      new <name> <file_template> -j [parameters_string] -- create file from the template with specified JSON parameters, wrap it with "" or ''
      new <name> <file_template> [parameters] -- create file from the template with specified string parameters
      list -- print all available templates
      help <template> -- print template info
      help -- print this help
      version -- print version
  
* Command 'update' fetches all the latest templates from the repository. Run it before using any other command.
* Command 'list' shows available templates. Technically it finds all the files *.metainfo in $HOME/.trurl/repo and prints thir first lines.
* Command 'help <template>' prints detailed info about the template.
* Command 'create' intended to generate projects, just specify an available project template name. You may also sepcify optional parameters. In any case at least one parameter will be available in project template -- 'ProjectName' which corresponds to the provided <name> parameter.
* Command 'new' generates template file, generated file will be named as specified in 'name'. It uses Mustache format in templates and accepts mandatory parameters. In any case at least one parameter will be available in the file template -- 'FileName' which corresponds to the provided <name> parameter.

Parameters for commands 'create' and 'new' may be of two types: JSON and simple string.

* JSON parameters might look like this: '{"entityName": "Article", "params": [ {"name":"title","type":"String"}, {"name":"body", "type":"String"}]}"
* Totally the same simple string looks like this: 'entityName:Article, params: [ title#String, body#String ]'

Simple string parameters use following rules to correspond to JSON:

* abc:efg is converted to "abc":"efg"
* abc:123 is converted to "abc":123
* abc#efg is converted to {"name":"abc", "type":"efg"} -- this conversion is especially useful when generating object with list of properties.
* abc#efg@ is converted to {"name":"abc", "type":"efg", "last":true} -- this conversion is needed when generating list of properties and you need to avoid last separator (comma, whitespace, etc).

For example, if there is a template file 'file1.txt' with following contents:

```
The list: 

{{#heroes}}
    * {{name}}
{{/heroes}}

MyObj:
{{#myobj}}
{{opt1}}
{{/myobj}}
```

Run trurl as following:

    trurl new file1.txt -j '{"heroes":[{"name":"1"},{"name":"22"}],"myobj":{"opt1":"value1"}}'

Then you'll get:

```
The list: 

    * 1
    * 22

MyObj:
value1
```

## Creating templates

All the templates are stored in $HOME/.trurl/repo. There are two types of templates: projects and files.

* Project template is just a tar archive which is unpacked in specified directory. Template files should have extension '.template' and use the same mustache syntax as templates for command 'new'. In any case at least one parameter will be available in project template -- 'ProjectName' which corresponds to the provided <name> parameter. All files named as 'ProjectName' will be renamed according to specified project name. E.g. if creating project 'my' and there is somewhere file 'ProjectName.html' then it will be renamed to 'my.html'.
* File template is a file in Mustache format. It may have any extension, but if not supplied while running 'trurl new ...' then extension '.hs' will be used.


Info about each template is stored in corresponding metainfo file. E.g. if there is template 'file1.hs' then should be also 'file1.hs.metainfo'. First line of the metainfo file is its short description printed by the 'list' command.

See examples of the templates in 'devrepo'.

## Registering templates in the main repository.

Just add a pull request in the directory 'devrepo'. You are welcome to add your own templates here!

Author: Dmitry Bushenko (d.bushenko@gmail.com)
