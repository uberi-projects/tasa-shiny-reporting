// Lamp Image Changing---------------------
function switchLampImage(isConch,isMulti) {
    if (isMulti) {
        elem = document.getElementById("datatype-lamp-multi-img")
    } else {
        elem = document.getElementById("datatype-lamp-1per-img")
    }
    if (isConch) {
        elem.setAttribute("src","../images/photos/photo_lamp_conch_03.jpg")
    } else {
        elem.setAttribute("src","../images/photos/photo_lamp_work_01.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeLampImg", function(message) {
    switchLampImage(message.isConch, message.isMulti);
});
// SPAG Image Changing---------------------
function switchSpagImage(isVisual, isMulti) {
    if (isMulti) {
        elem = document.getElementById("datatype-spag-multi-img")
    } else {
        elem = document.getElementById("datatype-spag-1per-img")
    }
    if (isVisual) {
        elem.setAttribute("src","../images/photos/photo_spags_02.png")
    } else {
        elem.setAttribute("src","../images/photos/photo_spags_06.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeSpagImg", function(message) {
    switchSpagImage(message.isVisual, message.isMulti);
});
// Animate Loader Bar
function updateLoader(reportType, percentage) {
    let progressBar = document.getElementById(reportType+"_loader_bar")
    progressBar.style.width = percentage+"%"
}
Shiny.addCustomMessageHandler('updateLoader', function(message) {
    updateLoader(message.reportType, message.percentage);
});