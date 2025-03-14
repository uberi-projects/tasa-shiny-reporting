// Lamp Image Changing
function changeLampImg(isConch) {
    if (isConch) {
        document.getElementById("datatype-img").setAttribute("src","../images/photos/photo_lamp_conch_03.jpg")
    } else {
        document.getElementById("datatype-img").setAttribute("src","../images/photos/photo_lamp_work_01.jpg")
    }
}

Shiny.addCustomMessageHandler("triggerChangeLampImg", function(message) {
    changeLampImg(message.isConch);
});

function changeLampMultiperImg(isConch) {
    if (isConch) {
        document.getElementById("datatype-multiper-img").setAttribute("src","../images/photos/photo_lamp_conch_03.jpg")
    } else {
        document.getElementById("datatype-multiper-img").setAttribute("src","../images/photos/photo_lamp_work_01.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeLampMultiPerImg", function(message) {
    changeLampMultiperImg(message.isConch);
});



// SPAG Image Changing
function changeSpag1perImg(isVisual) {
    if (isVisual) {
        document.getElementById("datatype-spag-1per-img").setAttribute("src","../images/photos/photo_spags_02.png")
    } else {
        document.getElementById("datatype-spag-1per-img").setAttribute("src","../images/photos/photo_spags_06.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeSpag1perImg", function(message) {
    changeSpag1perImg(message.isVisual);
});


function changeSpagMultiperImg(isVisual) {
    if (isVisual) {
        document.getElementById("datatype-spag-multi-img").setAttribute("src","../images/photos/photo_spags_02.png")
    } else {
        document.getElementById("datatype-spag-multi-img").setAttribute("src","../images/photos/photo_spags_06.jpg")
    }
}
Shiny.addCustomMessageHandler("triggerChangeSpagMultiperImg", function(message) {
    changeSpagMultiperImg(message.isVisual);
});