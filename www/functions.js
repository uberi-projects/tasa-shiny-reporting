// FISHER Image Changing---------------------
function switchFisherImage(dataType, isMulti) {
    if (isMulti) {
        elem = document.getElementById("datatype-fisher-multi-img")
    } else {
        elem = document.getElementById("datatype-fisher-1per-img")
    }
    switch (dataType) {
        case "Lobster": 
            elem.setAttribute("src","../images/photos/photo_catch_03.jpg")
        break;
        case "Conch": 
            elem.setAttribute("src","../images/photos/photo_catch_07.jpg")
        break;
        case "Finfish": 
            elem.setAttribute("src","../images/photos/photo_catch_09.jpeg")
        break;
        default:
            elem.setAttribute("src","../images/photos/photo_catch_03.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeFisherImg", function(message) {
    switchFisherImage(message.dataType, message.isMulti);
});
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
// Reset Loader
function resetLoader(reportType) {
    let progressBar = document.getElementById(reportType+"_loader_bar")
    progressBar.style.width = "0%"
}
Shiny.addCustomMessageHandler('resetLoader', function(message) {
    resetLoader(message.reportType);
});