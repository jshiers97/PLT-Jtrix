#J-Trix

#Team Members
Manager: Azim Djuraev
Language Guru: Jacob Shiers
System Architect: Levi Beckett
Tester: Arick Huang

#Useful Git Commands
* git fetch --all
    * Allows you to get all relevant updates to all branches since your last fetch
* git branch -a
    * Lists all branches on remote (in the github repo) and on your machine
* git status
    * Tells you how far you are in terms of commits from the branch, what files have been changed, what files have been staged for commmit, and what files have been newly added
* git pull
    * Retrieves changes in branch
* git add [filename/folder]
    * Stages file or folder for commit
* git checkout [filename/folder]
    * Reverts file or folder to what it was before you changed it if in the remote branch repo
* git checkout [branchname]
    * Checks out branch
* git checkout -b [branchname]
    * Creates new branch in local
* git commit -m [message]
    * Commits files with a comment
* git push
    * Pushes changes onto remote branch
* git push -u origin [branchname]
    * Creates new branch in remote

# Typical Conventions for Gitflow
## Branch naming
When you make a new branch, make sure to specify the purpose of it in the branch name (i.e. I want to make a parser, so I would make a branch called `feature-parser`).

## More to come later