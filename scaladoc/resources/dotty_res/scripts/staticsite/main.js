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
