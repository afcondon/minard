import * as vscode from 'vscode';
import * as path from 'path';

export function activate(context: vscode.ExtensionContext) {
  console.log('Code Explorer Link extension activated');

  const cmd = vscode.commands.registerCommand('codeExplorer.showInExplorer', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showWarningMessage('No active editor');
      return;
    }

    const filePath = editor.document.uri.fsPath;
    const moduleName = deriveModuleName(filePath);

    if (!moduleName) {
      vscode.window.showWarningMessage('Could not determine module name from file path');
      return;
    }

    const url = `http://localhost:3000/?focus=${encodeURIComponent(moduleName)}`;
    console.log(`Opening Code Explorer: ${url}`);

    await vscode.env.openExternal(vscode.Uri.parse(url));
  });

  context.subscriptions.push(cmd);
}

function deriveModuleName(filePath: string): string | null {
  // PureScript: src/CE2/Component/App.purs -> CE2.Component.App
  const pursMatch = filePath.match(/src\/(.+)\.purs$/);
  if (pursMatch) {
    return pursMatch[1].replace(/\//g, '.');
  }

  // Haskell: src/MyModule/Thing.hs -> MyModule.Thing
  const hsMatch = filePath.match(/src\/(.+)\.hs$/);
  if (hsMatch) {
    return hsMatch[1].replace(/\//g, '.');
  }

  // Fallback: just use filename without extension
  const ext = path.extname(filePath);
  const base = path.basename(filePath, ext);
  return base;
}

export function deactivate() {}
