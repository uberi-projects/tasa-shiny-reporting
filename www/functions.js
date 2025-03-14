function changeLampImg(isConch, isMulti) {
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