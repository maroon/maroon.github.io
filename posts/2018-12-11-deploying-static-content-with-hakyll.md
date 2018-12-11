---
title: Deploying Static Content with Hakyll
date: 2018-12-11 14:55:54 -0600
tags: haskell, hakyll
---

When it comes time to deploy a Hakyll project using a service such as GitHub Pages, there is a tutorial [just for that.](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html)

Of course I wouldn't be writing this if it had worked for me. I suspected it might not work out when I saw the files being copied between branches. For most, this solution may work without an issue. For myself, it was a constant source of pain. During the design phase of my project I realized that there should never be a reason to have the static web content follow the same flow of my development branch. It felt odd to have two disparate patterns inhabiting the same path. To facilitate this behavior I created the `master` as an orphaned branch.

Because of this design decision, the helper script wouldn't play nicely with the differences between branches. I had to update the `master`'s `.gitignore` file and adjust the `rsync` command to remove the unwanted files that remained during any branch checkouts.

``` zsh
rsync -a --filter='P _site/'       \
         --filter='P _cache/'      \
         --filter='P .git/'        \
         --filter='P .gitignore'   \
         --delete-excluded         \
         _site/ .
```

This is an abridged version of the `rsync` necessary to achieve what I wanted. It is important to note that there are changes to the command that caused additional side effects which are not present within the tutorial link above.

When using the `rsync` command from the tutorial I noticed several of my development files would "leak" over to the `master` branch, which I did not want. One option was to change the `.gitignore` file within the `master` branch to exclude these development related files. The problem that I faced was that some structures, such as the `post` directory, were shared between the `master` and development branches and could potentially require complex exclusion patterns. Therefore, I chose a different option: I instead decided to modify the `rsync` command in the deployment script since such structures would be overridden during its execution.

Obviously this became increasingly painful to maintain as the site grew in complexity. In addition to the script's frailty, I often had to combat data loss when I failed to append additional filter commands for new files or directories. The script did hold together while I deployed the first few updates to the site. Once I was satisfied with how the site worked, I turned my attention to creating a better deployment solution for myself.

`git` has a command called `worktree` that is defined as allowing the management of multiple working trees attached to the same repository. This amounts to performing a `checkout` on a branch which is then placed into a different directory than that of the working directory. As an aside, this is [often](https://git-scm.com/docs/git-worktree/2.20.0#_bugs) a fantastic command for avoiding the typical `stash`/`commit`/`rebase` necessary to get the working directory back to its previous state after an emergency branch change.

``` zsh
git worktree remove -f _master

git stash -u
git checkout devel
./scripts/rebuild_site.sh

git worktree add _master master
git fetch --all
(cd _master && git merge --ff-only)

rsync -a _site/ _master/

(cd _master && git add --all && git commit -m "$1")

git worktree remove -f _master
```

Here is the core of the deployment script I came up with. Depending on workflow requirements some modifications to fetching and fast-forwarding might be necessary. Keep in mind that I haven't had a reason to test this on a non-orphaned Hakyll project, although I would imagine that it should work in a similar fashion to the tutorial linked herein.

First it ensures that no work tree exists within the `_master` directory; if there is, any unsaved changes within the directory are lost. The steps following that are pretty straightforward: stash all changes, checkout the development branch, and preform a rebuild of the static web content. After that the `worktree` command is leveraged: it creates a working tree of the `master` branch inside the `_master` directory. The latest remote changes are fetched and the local `master` repository is fast-forwarded. It then issues an `rsync` command that copies the static web content contained within the `_site` directory into the `_master` directory. All the changes within `_master` are then staged and committed. Finally, the working tree is removed, leaving the development branch in a clean state and any unsaved changes at the top of the stash.

``` zsh
→ ./scripts/deploy.sh "Commit message goes here!"
```

This is an example of a call to the deployment script. Note the argument given to it is passed along as the `master`'s commit message. After the script has finished I review the changes made to the `master` branch. If all looks well, I then push those changes to the remote repository.
