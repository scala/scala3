;(function () {
  const supportsLocalStorage = (() => {
    try {
      localStorage.setItem('test', 'test');
      localStorage.removeItem('test');
      return true;
    } catch (e) {
      return false;
    }
  })();

  function toggleDarkTheme(isDark) {
    currentlyDark = isDark
    // this triggers the `:root.theme-dark` rule from scalastyle.css,
    // which changes the values of a bunch of CSS color variables
    document.documentElement.classList.toggle("theme-dark", isDark);
    supportsLocalStorage && localStorage.setItem("use-dark-theme", isDark);
  }

  /* This needs to happen ASAP so we don't get a FOUC of bright colors before the dark theme is applied */
  const initiallyDark = supportsLocalStorage && (localStorage.getItem("use-dark-theme") === "true");
  let currentlyDark = initiallyDark;
  toggleDarkTheme(initiallyDark);

  /* Wait for the DOM to be loaded before we try to attach event listeners to things in the DOM */
  window.addEventListener("DOMContentLoaded", () => {
    const themeToggler = document.querySelector('#theme-toggle input');
    themeToggler.checked = !currentlyDark;
    themeToggler.addEventListener("change", e => {
      toggleDarkTheme(!e.target.checked);
    });
  });
})();
