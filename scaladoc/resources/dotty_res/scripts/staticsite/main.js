// Get all the alt-details-control elements
const altDetailsControls = document.querySelectorAll('.alt-details-control');

// Loop through each control element
altDetailsControls.forEach(control => {
  // Add event listener for 'change' event
  control.addEventListener('change', function () {
    // Get the corresponding alt-details-detail element
    const detailElement = this.nextElementSibling.nextElementSibling;

    // Toggle the display of the detail element based on checkbox state
    if (this.checked) {
      detailElement.style.display = 'block';
    } else {
      detailElement.style.display = 'none';
    }
  });
});


// JavaScript for Tab Functionality
document.addEventListener("DOMContentLoaded", function () {
  const tabLinks = document.querySelectorAll(".tab-link");
  const tabContents = document.querySelectorAll(".tab-content");

  tabLinks.forEach(link => {
    link.addEventListener("click", function () {
      const targetId = this.getAttribute("data-tab");

      tabLinks.forEach(l => l.classList.remove("active"));
      this.classList.add("active");

      tabContents.forEach(content => {
        if (content.id === targetId) {
          content.style.display = "block";
        } else {
          content.style.display = "none";
        }
      });
    });
  });

  // Activate the first tab by default
  if (tabLinks.length > 0) {
    tabLinks[0].click();
  }
});
