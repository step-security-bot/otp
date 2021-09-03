
function toggleDisplay() {
    var leftnav=document.getElementById('leftnav');
    var topbar=document.getElementsByClassName('topbar-expand')[0];
    if (leftnav.classList.contains('show')) {
        leftnav.classList.remove('show');
        topbar.classList.remove('show');
    } else {
        leftnav.classList.add('show');
        topbar.classList.add('show');
    }
}
