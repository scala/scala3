/* Interactive toggles for experimental language features (e.g. capture checking).
 *
 * Signature fragments that depend on such a feature are rendered twice by
 * scaladoc: a `.feature-on` variant (with the feature's annotations) and a
 * `.feature-off` variant (without them), wrapped in a `.feature-<id>` span.
 * CSS displays exactly one of the two variants, keyed on a `<id>-hidden`
 * class on the root element that is maintained here.
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

  const features = [
    {
      name: "capture checking",
      storageKey: "hide-cc",
      rootClass: "cc-hidden",
      toggleId: "cc-toggle",
      mobileToggleId: "mobile-cc-toggle",
    },
  ];

  features.forEach(feature => {
    let hidden =
      supportsLocalStorage && localStorage.getItem(feature.storageKey) === "true";

    /* Applied ASAP so we don't get a flash of feature-specific content before
     * the stored preference kicks in */
    document.documentElement.classList.toggle(feature.rootClass, hidden);

    window.addEventListener("DOMContentLoaded", () => {
      const toggle = document.getElementById(feature.toggleId);
      const mobileToggle = document.getElementById(feature.mobileToggleId);

      function render() {
        document.documentElement.classList.toggle(feature.rootClass, hidden);
        if (toggle !== null) {
          toggle.classList.toggle("feature-toggle-off", hidden);
          toggle.setAttribute("aria-pressed", !hidden);
        }
        if (mobileToggle !== null) {
          mobileToggle.textContent = (hidden ? "Show " : "Hide ") + feature.name;
        }
      }

      function flip() {
        hidden = !hidden;
        supportsLocalStorage && localStorage.setItem(feature.storageKey, hidden);
        render();
        /* Let other components (e.g. the inheritance diagram, whose labels are
         * measured at render time) react to the changed feature state */
        window.dispatchEvent(new CustomEvent("feature-toggled"));
      }

      toggle && toggle.addEventListener("click", flip);
      mobileToggle && mobileToggle.addEventListener("click", flip);
      render();
    });
  });
})();
