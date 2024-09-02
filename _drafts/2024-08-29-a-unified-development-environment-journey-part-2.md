---
layout: post
title: "A Unified Development Environment Journey - Part 2"
description: "My dream about a unified development environment for everybody in the team for the last 10 years..."
categories: [misc]
tags: []
---

> Previous post: [A Unified Development Environment Journey - Part 1]({% post_url 2024-08-26-a-unified-development-environment-journey-part-1 %})

I won't mention about setting up Codespaces or Devcontainer as it's already on their documentation
page. In this post, I'll show you some of the problems and example workflows that I did with
Codespaces/Devcontainer.

# Dev credentials and Environment variables

Your organization may created some private packages that requires some layers of authentication in
order to install (the npm packages hosted on GCP for example). One possible solution is to create
an access token (or any other type of credential) and put set it in your development environment.
For Codespaces, you can simply set it on your Github configuration page. However, for Devcontainer,
it needs a bit more work. The idea is to mount the environment file from outside of the container
and then load those variables to Devcontainer

Create an env file on your computer, for example `/var/devcontainer-mount/workflow.env`, which
contains all the secrets that you want to inject
```bash
export GOOGLE_APPLICATION_CREDENTIALS="xxx"
```

Mount the folder to your Devcontainer by adding a mount volume to your **devcontainer.json** file.
Read more here [Add another local file mount](https://code.visualstudio.com/remote/advancedcontainers/add-local-file-mount)
```json
{
  //...other config
  "mounts": [
    {
      "source": "/var/devcontainer-mount",
      "target": "/workspaces/devcontainer-mount",
      "type": "bind"
    }
  ]
}
```

In your Devcontainer bootstrap file (usually defined under `updateContentCommand` section in your
**devcontainer.json** file), load those variables before executing any script
```bash
if [[ -e "/workspaces/devcontainer-mount/workflow.env" ]]; then
  echo "Sourcing secret env file..."
  source "/workspaces/devcontainer-mount/workflow.env"
fi
```

You can also make these variables available by default in your VSCode Terminal or in SSH session
by adding it to your shell's startup files. For example, if you use zsh with a Node Devcontainer,
add this to your bootstrap file
```bash
touch /home/node/.zshenv
cat /workspaces/devcontainer-mount/workflow.env >> /home/node/.zshenv
```

# Git config not shared properly with Devcontainer

It's suggested that you install [git-credential-manager](https://github.com/git-ecosystem/git-credential-manager)
when working with Devcontainer. However, it only transfers the credential, not the configurations (git username and user email).
Codespaces set these values automatically based your Github account. For Devcontainer, you need
to do it manually.

Continue with the `workflow.env` file that I mentioned above, add your Git details
```bash
GIT_USER_NAME="Tony Tran"
GIT_USER_EMAIL="my.email@my-company.com"
```

and then in your bootstrap file (after sourcing the env file)
```bash
git config user.name > /dev/null || gitUserNameCheck=$?
if [[ -n "$gitUserNameCheck" && -n "$GIT_USER_NAME" ]]; then
  echo "Setting Git user.name"
  git config --global user.name "$GIT_USER_NAME"
fi

git config user.email > /dev/null || gitUserEmailCheck=$?
if [[ -n "$gitUserEmailCheck" && -n "$GIT_USER_EMAIL" ]]; then
  echo "Setting Git user.email"
  git config --global user.email "$GIT_USER_EMAIL"
fi
```