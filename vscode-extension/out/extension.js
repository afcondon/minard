"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const path = __importStar(require("path"));
function activate(context) {
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
function deriveModuleName(filePath) {
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
function deactivate() { }
//# sourceMappingURL=extension.js.map