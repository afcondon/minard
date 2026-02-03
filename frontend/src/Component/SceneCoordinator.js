// SceneCoordinator FFI - Browser History API integration
// Enables back/forward buttons to navigate within the app

// Push a scene to browser history
// pushHistoryState :: String -> Effect Unit
export const pushHistoryState = (sceneJson) => () => {
  // Parse the scene JSON to create a meaningful URL
  // For now, just store the scene in state
  const state = { scene: sceneJson };
  window.history.pushState(state, '', null);
};

// Replace the current history state (for initial setup)
// replaceHistoryState :: String -> Effect Unit
export const replaceHistoryState = (sceneJson) => () => {
  const state = { scene: sceneJson };
  window.history.replaceState(state, '', null);
};

// Set up a popstate listener that calls back with the scene JSON
// Returns an unsubscribe function
// setupPopstateListener :: (String -> Effect Unit) -> Effect (Effect Unit)
export const setupPopstateListener = (callback) => () => {
  const handler = (event) => {
    if (event.state && event.state.scene) {
      callback(event.state.scene)();
    }
  };
  window.addEventListener('popstate', handler);

  // Return cleanup function
  return () => {
    window.removeEventListener('popstate', handler);
  };
};
