// JavaScript for Tab Functionality
document.addEventListener("DOMContentLoaded", function() {
  const tabLinks = document.querySelectorAll(".tab-link");
  const tabContents = document.querySelectorAll(".tab-content");

  tabLinks.forEach(link => {
    link.addEventListener("click", function() {
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
