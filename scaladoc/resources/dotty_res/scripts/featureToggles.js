/* Interactive toggles for experimental language features (e.g. capture checking).
 *
 * Signature fragments that depend on such a feature are rendered twice by
 * scaladoc: a `.feature-on` variant (with the feature's syntax) and a
 * `.feature-off` variant (without it), wrapped in a `.feature-<id>` span.
 * CSS displays exactly one of the two variants, keyed on a `<id>-hidden`
 * class on the root element that is maintained here. Pages containing such
 * fragments carry a switch, `input[data-feature-toggle="<id>"]`, below the
 * main signature.
 *
 * Like theme.js, this script is loaded without `defer` so the stored
 * preference is applied before first paint.
 */
; (function () {
  const supportsLocalStorage = (() => {
    try {
      localStorage.setItem('test', 'test');
      localStorage.removeItem('test');
      return true;
    } catch (e) {
      return false;
    }
  })();

  const root = document.documentElement;

  const features = ["cc"].map(id => ({
    id,
    storageKey: `hide-${id}`,
    rootClass: `${id}-hidden`,
    selector: `input[data-feature-toggle="${id}"]`,
  }));

  /* Applies the stored preferences and returns the features whose state
   * changed. Only an explicit choice of the reader is stored, so the default
   * may change without overriding it. */
  function applyStoredPreferences() {
    return features.filter(feature => {
      const hidden =
        supportsLocalStorage && localStorage.getItem(feature.storageKey) === "true";
      const changed = root.classList.contains(feature.rootClass) !== hidden;
      root.classList.toggle(feature.rootClass, hidden);
      return changed;
    });
  }

  /* Applied ASAP so we don't get a flash of feature-specific content before
   * the stored preference kicks in */
  applyStoredPreferences();

  /* The switches are rendered checked; bring them in line with the root
   * classes, also after a dynamic page load replaced the content */
  function syncSwitches() {
    features.forEach(feature => {
      const shown = !root.classList.contains(feature.rootClass);
      document.querySelectorAll(feature.selector).forEach(input => {
        input.checked = shown;
      });
    });
  }

  /* As soon as the document is parsed, i.e. before the deferred scripts run */
  document.addEventListener("readystatechange", () => {
    if (document.readyState === "interactive") syncSwitches();
  });
  window.addEventListener("dynamicPageLoad", syncSwitches);

  function featureToggled(feature) {
    syncSwitches();
    /* Let other components (e.g. the inheritance diagram, whose labels are
     * measured at render time) react to the changed feature state */
    const hidden = root.classList.contains(feature.rootClass);
    window.dispatchEvent(
      new CustomEvent("feature-toggled", { detail: { feature: feature.id, hidden } }),
    );
  }

  /* A single delegated listener, since the content holding the switches is
   * replaced on dynamic page loads */
  document.addEventListener("change", e => {
    const target = e.target;
    const feature =
      target instanceof Element && features.find(f => target.matches(f.selector));
    if (!feature) return;

    /* Flip what the reader sees: until the switches are synced, their checked
     * state may disagree with the root class */
    const hidden = !root.classList.contains(feature.rootClass);
    root.classList.toggle(feature.rootClass, hidden);
    supportsLocalStorage && localStorage.setItem(feature.storageKey, hidden);
    featureToggled(feature);
  });

  /* Pages restored from the back/forward cache and pages in other tabs follow
   * choices made elsewhere */
  function reapplyStoredPreferences() {
    applyStoredPreferences().forEach(featureToggled);
  }
  window.addEventListener("pageshow", e => {
    if (e.persisted) reapplyStoredPreferences();
  });
  window.addEventListener("storage", e => {
    if (e.key === null || features.some(f => f.storageKey === e.key)) reapplyStoredPreferences();
  });
})();
