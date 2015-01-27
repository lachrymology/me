var loadingImage = null;

function showZoomedCover(mediumId) {
    var zoomElement = document.getElementById("zoom");
    var imageNode = document.getElementById("zoom_image");
    var shouldHide = (zoomElement.style.visibility == "visible");
    
    var imageFileName = "images/" + mediumId + "-256.png";
    if (imageNode.src.indexOf(imageFileName) == -1) { // changed image source
        shouldHide = false;
        imageNode.src = imageFileName;
    }
    
    zoomElement.style.visibility = shouldHide ? "hidden" : "visible";
    var medium = document.getElementById("medium" + mediumId);
    var asin = null;
    if (medium)
        asin = medium.getElementsByClassName("asin")[0].innerHTML;
    if (asin)
        document.getElementById("zoom_link").href = "http://www.amazon.com/gp/reader/" + asin;
}

var mediumAnimatorChains = new Array();
var selectedMediumDetailsButton = null;

function mediumElementForButton(button) {
    var medium = button.parentNode.parentNode;
    if (medium.className != "medium")
        return null;
    
    return medium;
}

function clickMediumDetailsButton(button) {
    var medium = mediumElementForButton(button);
    
    if (selectedMediumDetailsButton && button != selectedMediumDetailsButton) // deselect the other medium
        clickMediumDetailsButton(selectedMediumDetailsButton);
    
    var isNewSelection = (selectedMediumDetailsButton == null);
    selectedMediumDetailsButton = isNewSelection ? button : null; // select or deselect ourselves
    button.src = !isNewSelection ? "images/moredetails-close.png" : "images/moredetails-open.png";
    
    if (!mediumAnimatorChains[medium.id])
        mediumAnimatorChains[medium.id] = new AnimatorChain(new Array(mediumAnimator(medium), detailsAnimator(medium)), {resetOnPlay: false});

    mediumAnimatorChains[medium.id].toggle();
}

function mediumAnimator(medium) {
    if (!medium.id)
        return null;
    
    var mediumAnimator = Animator.apply(medium, "medium_selected", {interval:10, duration:200}).addSubject(new DiscreteStyleSubject(medium, '-webkit-box-shadow', "", "0px 1px 5px rgba(0, 0, 0, 0.5)", 0.5))
    for (var elementIndex = 0; elementIndex < medium.childNodes.length; elementIndex++) {
        var element = medium.childNodes[elementIndex];
        if (!element.className || element.className.length < 1)
            continue;
        if (element.className == 'cover')
            mediumAnimator.addSubject(new CSSStyleSubject(element, "cover_selected"));
        else if (element.className == 'creator')
            mediumAnimator.addSubject(new CSSStyleSubject(element, "creator_selected"));
        else if (element.className == 'title')
            mediumAnimator.addSubject(new CSSStyleSubject(element, "title_selected"));
    }
    
    return mediumAnimator;
}

function detailsAnimator(medium) {
    if (!medium.id)
        return null;
    
    var detailsAnimator;
    var listOfChildElements = medium.getElementsByTagName("div");
    for (var elementIndex = 0; elementIndex < listOfChildElements.length; elementIndex++) {
        var element = listOfChildElements[elementIndex];
        if (!element.className || element.className.length < 1)
            continue;
        if (element.className == 'description') {
            detailsAnimator = Animator.apply(element, "description_selected", {interval:10, duration:30});
            break;
        }
    }
    
    return detailsAnimator;
}
