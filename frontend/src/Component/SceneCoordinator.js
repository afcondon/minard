// SceneCoordinator FFI - Browser History API integration
// Enables back/forward buttons to navigate within the app

// Push a scene + viewMode to browser history
// pushHistoryState :: String -> String -> Effect Unit
export const pushHistoryState = (sceneJson) => (viewModeStr) => () => {
  const state = { scene: sceneJson, viewMode: viewModeStr };
  window.history.pushState(state, '', null);
};

// Replace the current history state (for initial setup)
// replaceHistoryState :: String -> String -> Effect Unit
export const replaceHistoryState = (sceneJson) => (viewModeStr) => () => {
  const state = { scene: sceneJson, viewMode: viewModeStr };
  window.history.replaceState(state, '', null);
};

// Set up a popstate listener that calls back with scene JSON and viewMode string
// Returns an unsubscribe function
// setupPopstateListener :: (String -> String -> Effect Unit) -> Effect (Effect Unit)
export const setupPopstateListener = (callback) => () => {
  const handler = (event) => {
    if (event.state && event.state.scene) {
      callback(event.state.scene)(event.state.viewMode || "primary")();
    }
  };
  window.addEventListener('popstate', handler);

  // Return cleanup function
  return () => {
    window.removeEventListener('popstate', handler);
  };
};
