// // The module 'vscode' contains the VS Code extensibility API
// // Import the module and reference it with the alias vscode in your code below
// import * as vscode from 'vscode';

// // This method is called when your extension is activated
// // Your extension is activated the very first time the command is executed
// export function activate(context: vscode.ExtensionContext) {

// 	// Use the console to output diagnostic information (console.log) and errors (console.error)
// 	// This line of code will only be executed once when your extension is activated
// 	console.log('Congratulations, your extension "my-extension-ls" is now active!');

// 	// The command has been defined in the package.json file
// 	// Now provide the implementation of the command with registerCommand
// 	// The commandId parameter must match the command field in package.json
// 	const disposable = vscode.commands.registerCommand('my-extension-ls.helloWorld', () => {
// 		// The code you place here will be executed every time your command is executed
// 		// Display a message box to the user
// 		vscode.window.showInformationMessage('Hello World from my-extension-ls!');
// 	});

// 	context.subscriptions.push(disposable);
// }

// // This method is called when your extension is deactivated
// export function deactivate() {}

import { LanguageClient } from  'vscode-languageclient/node';
import { ExtensionContext, workspace } from  'vscode';

var client: LanguageClient;

function activate(context: ExtensionContext) {
    // Define the server options
    // This is where you specify how to run YOUR executable
    const exePath = "/Users/kf/Documents/projects/uni/free-foil-ls/.stack-work/install/aarch64-osx/3146d80ab82fce8dc4fd9dfc1fb9ef15741c5c7341ba15c165ce4a568c264d55/9.10.3/bin/lampi-language-server-exe"
	// const exePath = binPath + "/hs-ls";

    const serverOptions = {
        run: {
            command: exePath, // Path to your binary
            // Alternatively, if it's in the project's node_modules or on the system PATH:
            // command: "your-language-server-name",
        },
        debug: {
            // You can specify additional arguments for debugging if needed
            command: exePath,
        }
    };

    // Define the client options
    // This specifies which files will trigger the language server
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'lampi' }], // e.g., { language: 'python' }
        synchronize: {
            // Synchronize the settings section to the server
            configurationSection: 'lampi',
            // Notify the server about file changes to contained files
            fileEvents: [
                workspace.createFileSystemWatcher('**/*.lampi'),
            ]
        }
    };

    // Create the language client
    client = new LanguageClient(
        'lampi', // A unique ID for your server
        'lampi', // The name that will be shown in the output
        serverOptions,
        clientOptions
    );

    // Start the client, which will also start the server
    client.start();
}

function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = {
    activate,
    deactivate
};