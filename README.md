# emacs
my emacs setup
name this repo as .emacs.d, and import it into your .emacs file like this:


```elisp
(load-file '~/.emacs.d/stephen.el')
```

You might need to install `use-package` and `exec-path-from-shell` beforehand.

# What I use Emacs for
* js / ts
* rust
* go

# emacs dependencies
* use-package : need this to get all the other dependencies!  just `package-install` it.

# outside of emacs dependencies
Highkey assumes you're using MacOSX. Sorry.

Other dependencies:
* fzf
* TODO: pbcopy (should probably make this os-agnostic soon)

# Codex + Emacs MCP

This config now starts the built-in Emacs server on boot, which is the main prerequisite for [`@keegancsmith/emacs-mcp-server`](https://github.com/keegancsmith/emacs-mcp-server).

The upstream MCP server is a small Node shim that shells out to `emacsclient --eval`. It does not replace `gptel` or `emacs-agent.el`; it gives external MCP clients such as Codex a narrow bridge into the currently selected Emacs window.

From Emacs, use these commands:

* `M-x stephen-emacs-mcp-doctor` to see whether `server-start`, `emacsclient`, and `npx` are available.
* `M-x stephen-emacs-mcp-copy-codex-config` to copy a Codex MCP config snippet to the kill ring.

Codex MCP server entry:

```json
{
  "mcpServers": {
    "emacs-mcp": {
      "command": "npx",
      "args": ["-y", "@keegancsmith/emacs-mcp-server"]
    }
  }
}
```

Practical shape of the integration:

* Keep using `gptel` and `emacs-agent.el` inside Emacs for in-editor workflows.
* Add the `emacs-mcp` server to Codex so Codex can inspect or act on the currently visible Emacs buffer through MCP.
* Make sure `emacsclient` and `npx` are on the app's PATH, otherwise the MCP server will start but fail to talk to Emacs.
