# Gitignore 

## Objectives

This project is intended to allow developers to manipulate their project's `.gitignore` file using the CLI, without having to open an editor.  

It's probably a bit of a niche need because people these days tend to write the content of such files directly using their own IDE or editor of choice.  

I, for one, tend to interact with files that are related to CLI tools (such as `git`) using `vim` to edit them, whereas to write source code in some programming language I tend to use an IDE (primarily VSCode or IntelliJ).  
There's no right or wrong to this; it's just down to individual routines and workflows. If this works for me, it doesn't mean it would be the right tool for you.

## What can I do with it?

If you download the compiled binary and install it somewhere in your user's `$PATH`, you should be able to invoke it and get a nice _help_ menu by passing in the relative flag:

    gitignore --help

This will briefly explain that you can:

#### Add a pattern to your project's .gitignore

This is simply achieved by invoking:

    gitignore add pattern

where *pattern* is any string that represents a valid file path (literal or *glob*). 

This will also ask you if you want to *create* a `.gitignore` file in your repository if you don't have one yet.

If the pattern you're trying to add already exists in the file, the application will not add it (but currently only if the pattern matches perfectly an existing entry in the file).

#### Remove a pattern from your project's .gitignore

Run:

    gitignore remove

You'll be presented with a list of entries in your `.gitignore` file, each of which is referenced with the relative line number. You'll be able to pick which pattern to remove or exit by pressing `0`.

#### Print the content of .gitignore

This can sometimes be useful if you don't know/remember what's already in it:

    gitignore print

## Releases

The application will hopefully be developed over time to a usable point, but in the meantime, you can download the Linux x64 binary from the [releases](https://github.com/FrancescoSerra/gitignore/releases) page.

## Contributions, ideas, issues

If you find any of this interesting or think it can be developed further, feel free to open an issue for a contribution idea or development proposal.