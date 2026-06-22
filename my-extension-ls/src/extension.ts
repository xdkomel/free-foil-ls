import { LanguageClient } from 'vscode-languageclient/node';
import { ExtensionContext, workspace } from 'vscode';

var client: LanguageClient;

function activate(context: ExtensionContext) {
    // Path to the language server executable. Replace with your own path.
    const exePath = "/Users/kf/Documents/projects/uni/free-foil-ls/.stack-work/install/aarch64-osx/3146d80ab82fce8dc4fd9dfc1fb9ef15741c5c7341ba15c165ce4a568c264d55/9.10.3/bin/lampi-language-server-exe"

    const serverOptions = {
        run: { command: exePath },
        debug: { command: exePath }
    };

    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'lampi' }],
        synchronize: {
            configurationSection: 'lampi',
            fileEvents: [workspace.createFileSystemWatcher('**/*.lampi')]
        }
    };

    client = new LanguageClient('lampi', 'lampi', serverOptions, clientOptions);
    client.start();
}

function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = { activate, deactivate };
