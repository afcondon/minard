-- | Shared annotation empty-state guide
-- |
-- | Rendered when a view has no annotations to display. Explains what
-- | annotations are, how to generate them, and the human/LLM review loop.
-- | Used by PackageReportViz, AnnotationReportViz, and ModuleStructureViz.
module CE2.Component.AnnotationGuide
  ( renderAnnotationGuide
  ) where

import Prelude ((<>))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Amber info box explaining the annotation system.
-- | Accepts a `compact` flag: when true, omits the curl examples
-- | (suitable for narrow panels like the module structure left column).
renderAnnotationGuide :: forall w i. { compact :: Boolean } -> HH.HTML w i
renderAnnotationGuide { compact } =
  HH.div
    [ HP.style "border: 1px solid #e8c87a; border-left: 4px solid #d4940a; border-radius: 4px; background: #fef9ed; padding: 20px 24px; font-size: 12px; line-height: 1.7; color: #665520;" ]
    [ HH.div [ HP.style "display: flex; align-items: flex-start; gap: 10px;" ]
        [ HH.span [ HP.style "font-size: 16px; line-height: 1; flex-shrink: 0; margin-top: 1px;" ] [ HH.text "\x2139\xFE0F" ]
        , HH.div_
            ( [ HH.div [ HP.style "font-weight: 600; margin-bottom: 6px; color: #8a6d1b; font-size: 13px;" ] [ HH.text "No annotations yet" ]
              , HH.p [ HP.style "margin: 0 0 10px 0;" ]
                  [ HH.text "Annotations are structured notes attached to modules and packages \x2014 architectural summaries, design rationale, review comments, and open questions. They can be written by LLM agents, humans, or both." ]
              , HH.p [ HP.style "margin: 0 0 10px 0;" ]
                  [ HH.text "Annotations support a review workflow: an LLM generates a summary, a human can "
                  , HH.span [ HP.style "font-weight: 600;" ] [ HH.text "confirm" ]
                  , HH.text ", "
                  , HH.span [ HP.style "font-weight: 600;" ] [ HH.text "contest" ]
                  , HH.text ", or "
                  , HH.span [ HP.style "font-weight: 600;" ] [ HH.text "reply" ]
                  , HH.text " with corrections. Replies form threaded conversations. When the underlying code changes, annotations are automatically flagged as "
                  , HH.span [ HP.style "font-style: italic;" ] [ HH.text "stale" ]
                  , HH.text " so both parties know to revisit them."
                  ]
              ] <> if compact then [] else
              [ HH.p [ HP.style "margin: 0 0 10px 0;" ]
                  [ HH.text "To generate annotations, point an LLM agent at the Minard API:" ]
              , HH.pre
                  [ HP.style "background: #fdf5dd; border: 1px solid #e8d8a0; border-radius: 3px; padding: 10px 14px; font-size: 11px; overflow-x: auto; color: #665520; margin: 0;" ]
                  [ HH.text "# List modules\ncurl http://localhost:3000/api/v2/modules\n\n# Read module source\ncurl 'http://localhost:3000/api/v2/module-source?module=MyApp.Component'\n\n# Post an annotation\ncurl -X POST http://localhost:3000/api/v2/annotations \\\n  -H 'Content-Type: application/json' \\\n  -d '{\"module\": \"MyApp.Component\", \"kind\": \"summary\",\n       \"source\": \"claude\", \"body\": \"...\"}'" ]
              ]
            )
        ]
    ]
