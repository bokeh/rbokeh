How to contribute to rbokeh
===========================

Thank you for sharing your code with the rbokeh. We appreciate your contribution!

## Join the mailing list

If you're not already on the Bokeh mailing list, take a minute to [join](https://groups.google.com/a/continuum.io/d/forum/bokeh).

## Check the issue tracker

Before you write too much code, check the [open issues in the rbokeh issue tracker](https://github.com/bokeh/rbokeh/issues?state=open)
to see if someone else has already filed an issue related to your work or is already working on it. If not, go ahead and
[open a new issue](https://github.com/bokeh/rbokeh/issues/new).

## Announce your work on the mailing list

Shoot us a quick email on the mailing list letting us know what you're working on. There
will likely be people on the list who can give you tips about where to find relevant
source or alert you to other planned changes that might effect your work.

If the work you're proposing makes substantive changes to rbokeh, you may be asked to attach a design document
to your issue in the issue tracker. This document should provide a high-level explanation of your design, clearly define the goal
of the new design, etc. This document is meant to save you time
as it allows the team a chance to provide feedback on the proposes changes. The document also allows the team to prepare for large changes to the code
base. We welcome change but also want to ensure that code quality is kept high.

## Submit your pull request

Github provides a nice [overview on how to create a pull request](https://help.github.com/articles/creating-a-pull-request).

Some general rules to follow:

* Do your work in [a fork](https://help.github.com/articles/fork-a-repo) of the rbokeh repo.
* Create a branch for each feature/bug in rbokeh that you're working on. These branches are often called "feature"
or "topic" branches.
* Use your feature branch in the pull request. Any changes that you push to your feature branch will automatically
be shown in the pull request.  If your feature branch is not based off the latest master, you will be asked to rebase
it before it is merged.
* If your pull request fixes an issue, reference the issue so that it will [be closed when your pull request is merged](https://github.com/blog/1506-closing-issues-via-pull-requests)
* Keep your pull requests as small as possible. Large pull requests are hard to review. Try to break up your changes
into self-contained and incremental pull requests, if need be, and reference dependent pull requests, e.g. "This pull
request builds on request #92. Please review #92 first."
* The first line of commit messages should be a short (<80 character) summary, followed by an empty line and then,
optionally, any details that you want to share about the commit.
* Include unit tests with your pull request. We love tests and [use Travis CI](https://travis-ci.org)
to check every pull request and commit.  We use the [testthat](http://adv-r.had.co.nz/Testing.html) R package for running unit tests.  See code in the `inst/tests` for examples.
