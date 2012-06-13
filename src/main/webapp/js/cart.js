function decrement(i){
    var cell = $new("td#item" + i);
    var original = cell.html().trim();
    var newValue = original - 1;
    cell.empty();
    cell.append(newValue);
}
$new(document).ready(function(){
    $new("ul#cartMenu").find("li a").each(function(){
        $new(this).click(function(){
            var activeOne = $new("ul#cartMenu li.active");
            activeOne.removeClass("active");
            $new(this).parent().addClass("active");
            var activeDiv = $new("#contents div.active");
            activeDiv.removeClass("active");
            var li = $new(this).parent();
            var id = li.attr("id");
            var idOfDiv = id.substring(0, id.length - 4);
            $new("#" + idOfDiv).addClass("active");
        })
    });
    $new("#codeInput").keypress(function(e){
        if(e.which === 13){
            $new("#codeInputClick").click();
        }
    });
});
