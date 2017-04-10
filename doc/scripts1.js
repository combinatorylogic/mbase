function srcref_over(dstid, id) {
    SvgConnectors.manage();
    let x = document.getElementById(dstid);
    document.getElementById('dummy_to').childNodes[0].removeAttribute('id');
    document.getElementById('dummy_from').childNodes[0].removeAttribute('id');
    x.childNodes[0].id = 'too';
    id.childNodes[0].id = 'frm';
    SvgConnectors.manage();
}

function srcref_leave(dstid, id) {
    SvgConnectors.manage();
    let x = document.getElementById(dstid);
    document.getElementById('dummy_to').childNodes[0].id = 'too';
    document.getElementById('dummy_from').childNodes[0].id = 'frm';
    x.childNodes[0].removeAttribute('id');
    id.childNodes[0].removeAttribute('id');
    SvgConnectors.manage();
}
