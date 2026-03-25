# Minard Teaser Walkthrough

## The path

1. **Galaxy Treemap** — let it land. It opens in topo-layer coloring by default (light→dark blue = shallow→deep dependency). Let the density register.
2. **Tap R** — reachability overlay (still on galaxy). What's used vs what's just present. Tap R again to return to topo view.
3. **Click a package** → Package Treemap. Pick something from the viz layer (Minard showing itself).
4. **Overlay showcase on Package Treemap** — cycle through in this order:
   - **P** — Purity (blue = pure, amber = effectful). Most immediately legible.
   - **R** — Reachability. What's actually used.
   - **H** — Changes. Git change frequency heat map.
   - **X** — Co-change. Modules that change together.
   Pause briefly on each to let the visual transformation register.
5. **Click a module** → Module Signature Map. Type signatures rendered inline.
6. **Click a declaration** → Declaration Detail. Arc diagram, purity coloring (blue/amber). Four clicks from universe to one function.
7. **Annotations** — LLM-generated summary, human reply, feedback thread.
8. **Breadcrumb back to galaxy.**
9. **Anatomy** — "That was the compiler's eye view. This is Spago's: what you own vs what you depend on, git, registry."

## Two-act structure

- **Act 1 (beats 1–7):** Vertical descent — compiler's eye view. Packages → modules → declarations → call graphs. Data from docs.json / corefn.json.
- **Act 2 (beat 8):** Lateral reframe — Spago's eye view. Workspace vs direct vs transitive deps. Git, files, registry.

## Key points to land

- Visualization density >> text/HTML. Look how much is legible on one screen.
- Easy to build in PureScript now because of Hylograph libraries.
- LLM integration: human sees overviews of LLM-generated code; LLM queries structure and generates images; annotations create a feedback loop between human and agent.
- Synthesizes multiple sources: Git, .spago, registry, compiler output, annotations.
- Extensible, open source, starting point not final word.
