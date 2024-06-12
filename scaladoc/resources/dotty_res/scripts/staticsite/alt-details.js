// Get all the alt-details-control elements
const altDetailsControls = document.querySelectorAll('.alt-details-control');

// Loop through each control element
altDetailsControls.forEach(control => {
    // Add event listener for 'change' event
    control.addEventListener('change', function() {
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
