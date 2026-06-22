# my-extension-ls

A minimal Visual Studio Code extension that hosts a language server built with the `free-foil-ls` library. The extension registers itself for a target language and forwards all LSP traffic to the server process — no language-specific logic lives in the extension itself.

Out of the box, it is wired for the **Lampi** language (`.lampi` files). Adapting it to **Flan** (`.flan`) or any other language requires two small edits described below.

## 1. Build the language server executable

Build your language server with either Stack or Cabal from the root of the `free-foil-ls` repository, depending on which build file your server project uses:

```bash
# Stack
stack build <your-server-package>

# Cabal
cabal build <your-server-package>
```

After a successful build, note the path to the produced executable. With Stack you can print it with:

```bash
stack exec -- which <your-server-exe>
```

## 2. Configure the extension

Open `src/extension.ts` and replace the hardcoded path on **line 8** with the path to your executable:

```ts
const exePath = "/path/to/your/language-server-exe"
```

Then verify that the language ID is consistent in both files:

- **`src/extension.ts`** — `documentSelector`, `configurationSection`, `fileEvents` glob, and the two `LanguageClient` constructor arguments all reference the language ID (currently `lampi`).
- **`package.json`** — `activationEvents`, `contributes.languages[].id`, and `contributes.languages[].extensions` must match the same ID and file extension.

For example, to switch to Flan, change `lampi` → `flan` and `*.lampi` → `*.flan` in both files.

## 3. Compile and run the extension

```bash
npm install
npm run compile
```

Then open the `my-extension-ls` folder in VS Code and press **F5** to launch the Extension Development Host.

## 4. Try it out

Open one of the sample workspaces in the Extension Development Host:

- `test-flan/` — contains a `.flan` source file
- `test-lampi/` — contains `.lampi` source files

With the extension running you should see go-to-definition, rename, hover with inferred types, squiggly-line diagnostics for type errors and unbound symbols, and syntax highlighting driven by semantic tokens — all provided by the language server, with no additional IDE glue in the extension.
