document.addEventListener("DOMContentLoaded", function () {
  const tabContainers = document.querySelectorAll('.tabs');
  tabContainers.forEach(container => {
    const radios = container.querySelectorAll('.tab-radio');
    const labels = container.querySelectorAll('.tab-label');
    const contents = container.querySelectorAll('.tab-content');

    // Hide all tab contents except the first
    contents.forEach((content, index) => {
      if (index !== 0) content.style.display = 'none';
    });

    // Check the first radio button
    if (radios.length > 0) radios[0].checked = true;
    contents[0].style.display = 'block'; // Ensure the first tab content is displayed

    labels.forEach((label, index) => {
      label.addEventListener('click', () => {
        // Hide all tab contents
        contents.forEach(content => content.style.display = 'none');

        // Show the clicked tab's content
        contents[index].style.display = 'block';
      });
    });
  });
});


function handleLanguageChange(selectElement) {
  console.log("This Function Works");

  var selectedLanguage = selectElement.value;
  var currentUrl = window.location.href;
  var urlParts = currentUrl.split('/');
  var baseUrl = urlParts.slice(0, 3).join('/');
  var pathParts = urlParts.slice(3);

  // Identify the index of the 'docs' path
  var docsIndex = pathParts.indexOf('docs');

  // Regex to match a language code right after the 'docs' path
  var languagePattern = new RegExp('^(' + availableLanguages.join('|') + ')(-[a-z]{2})?');
  var currentLangCode = docsIndex >= 0 && pathParts.length > docsIndex + 1 ? pathParts[docsIndex + 1].match(languagePattern) : null;

  if (selectedLanguage) {
    var updatedPath;

    if (selectedLanguage == 'en') {
      // If 'en' is selected, remove the language code if it exists after 'docs'
      if (currentLangCode && availableLanguages.includes(currentLangCode[0])) {
        updatedPath = pathParts.slice(0, docsIndex + 1).concat(pathParts.slice(docsIndex + 2)); // Remove the language code after 'docs'
      } else {
        updatedPath = pathParts;
      }
    } else {
      // If any other language is selected
      if (currentLangCode && availableLanguages.includes(currentLangCode[0])) {
        // Replace the existing language code with the new one after 'docs'
        updatedPath = pathParts.slice(0, docsIndex + 1).concat([selectedLanguage]).concat(pathParts.slice(docsIndex + 2));
      } else {
        // Add the new language code after the 'docs' path
        updatedPath = pathParts.slice(0, docsIndex + 1).concat([selectedLanguage]).concat(pathParts.slice(docsIndex + 1));
      }
    }

    // Handle edge case where updatedPath might be empty
    if (updatedPath.length === 0) {
      window.location.href = baseUrl;
    } else {
      window.location.href = baseUrl + '/' + updatedPath.join('/');
    }
  } else {
    // If no language is selected, keep the path unchanged
    window.location.href = baseUrl + '/' + pathParts.join('/');
  }
}
