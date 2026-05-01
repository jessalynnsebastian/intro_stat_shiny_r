from pathlib import Path

INDEX_PATH = Path('docs/index.html')

OLD_BLOCK = '''    <script type="module">
      import { runExportedApp } from "./shinylive/shinylive.js";
      runExportedApp({
        id: "root",
        appEngine: "r",
        relPath: "",
      });
    </script>'''

NEW_BLOCK = '''    <script type="module">
      import { runApp } from "./shinylive/shinylive.js";

      const response = await fetch(`./app.json?v=${Date.now()}`, {
        cache: "no-store",
      });

      if (!response.ok) {
        throw new Error("HTTP error loading app.json: " + response.status);
      }

      const appFiles = await response.json();
      runApp(document.getElementById("root"), "viewer", {
        startFiles: appFiles,
      }, "r");
    </script>'''

text = INDEX_PATH.read_text()
if OLD_BLOCK not in text:
    raise SystemExit('Expected Shinylive loader block not found in docs/index.html')

INDEX_PATH.write_text(text.replace(OLD_BLOCK, NEW_BLOCK))
