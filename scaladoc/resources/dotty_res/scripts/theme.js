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

  const settingKey = "use-dark-theme";

  function toggleDarkTheme(isDark) {
    currentlyDark = isDark
    // this triggers the `:root.theme-dark` rule from scalastyle.css,
    // which changes the values of a bunch of CSS color variables
    document.documentElement.classList.toggle("theme-dark", isDark);
    supportsLocalStorage && localStorage.setItem(settingKey, isDark);
  }

  /* Infer a dark/light theme preference from the user's system */
  const colorSchemePrefMql = window.matchMedia("(prefers-color-scheme: dark)");

  /* This needs to happen ASAP so we don't get a FOUC of bright colors before the dark theme is applied */
  const initiallyDark = (() => {
    const storedSetting = supportsLocalStorage && localStorage.getItem(settingKey);
    return (storedSetting === null) ? colorSchemePrefMql.matches : storedSetting === "true";
  })();
  let currentlyDark = initiallyDark;
  toggleDarkTheme(initiallyDark);

  /* Wait for the DOM to be loaded before we try to attach event listeners to things in the DOM */
  window.addEventListener("DOMContentLoaded", () => {
    const themeToggler = document.querySelector('#theme-toggle');
    themeToggler.addEventListener("click", e => {
      toggleDarkTheme(!currentlyDark);
    });

    /* Auto-swap the dark/light theme if the user changes it in their system */
    colorSchemePrefMql.addEventListener('change', e => {
      const preferDark = e.matches;
      toggleDarkTheme(preferDark);
    });
  });
})();
