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
  console.log("This Function Works")
  var selectedLanguage = selectElement.value;
  var currentUrl = window.location.href;
  var urlParts = currentUrl.split('/');
  var baseUrl = urlParts.slice(0, 3).join('/');
  var pathParts = urlParts.slice(3);

  // Regex to match a language code at the start of the path
  var languagePattern = /^[a-z]{2}(-[a-z]{2})?/;
  var currentLangCode = pathParts.length > 0 ? pathParts[0].match(languagePattern) : null;


  if (selectedLanguage) {
    var updatedPath;

    if (selectedLanguage == 'en') {
      // If 'en' is selected, remove the language code if it exists
      if (currentLangCode) {
        updatedPath = pathParts.length > 1 ? pathParts.slice(1) : [];
      } else {
        updatedPath = pathParts;
      }
    } else {
      // If any other language is selected
      if (currentLangCode) {
        // Replace the existing language code with the new one
        updatedPath = [selectedLanguage].concat(pathParts.slice(1));
      } else {
        // Add the new language code at the start
        updatedPath = [selectedLanguage].concat(pathParts);
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
