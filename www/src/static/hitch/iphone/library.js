// library.js
// Copyright 2007 Delicious Monster

if (!!navigator.userAgent.match(/iP*.*Mobile.*Safari/))
    addEventListener("load", function() { setTimeout(hideURLbar, 0); }, false);

function hideURLbar() {
    window.scrollTo(0, 1);
}

function mediumElementForButton(button) {
    var medium = button.parentNode.parentNode;
    if (medium.className != "medium" && medium.className != "medium_selected")
        return null;
    
    return medium;
}

var selectedMediumDetailsButton = null;

function clickMediumDetailsButton(button) {
    var medium = mediumElementForButton(button);
    
    if (selectedMediumDetailsButton && button != selectedMediumDetailsButton) // deselect the other medium
        clickMediumDetailsButton(selectedMediumDetailsButton);
    
    var isNewSelection = (selectedMediumDetailsButton == null);
    selectedMediumDetailsButton = isNewSelection ? button : null; // select or deselect ourselves
    button.src = !isNewSelection ? "../images/moredetails-close.png" : "../images/moredetails-open.png";
    
    mediumAnimator(medium, isNewSelection);
    detailsAnimator(medium, isNewSelection);
}

function mediumAnimator(medium, selected) {
    if (!medium || !medium.id)
        return;
    
    medium.style.webkitBoxShadow = selected ? "0px 1px 5px rgba(0, 0, 0, 0.5)" : "";
    medium.className = selected ? "medium_selected" : "medium";
    for (var elementIndex = 0; elementIndex < medium.childNodes.length; elementIndex++) {
        var element = medium.childNodes[elementIndex];
        if (!element.className || element.className.length < 1)
            continue;
        if (element.className == "cover" || element.className == "cover_selected")
            element.className = selected ? "cover_selected" : "cover";
        else if (element.className == "creator" || element.className == "creator_selected")
            element.className = selected ? "creator_selected": "creator";
        else if (element.className == "title" || element.className == "title_selected")
            element.className = selected ? "title_selected": "title";
    }
}

function detailsAnimator(medium, selected) {
    if (!medium || !medium.id)
        return;
    
    var detailsAnimator;
    var listOfChildElements = medium.getElementsByTagName("div");
    for (var elementIndex = 0; elementIndex < listOfChildElements.length; elementIndex++) {
        var element = listOfChildElements[elementIndex];
        if (!element.className || element.className.length < 1)
            continue;
        if (element.className == "description" || element.className == "description_selected") {
            element.className = selected ? "description_selected" : "description";
            break;
        }
    }
}
