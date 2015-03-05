function Chain(items){
    this.items = items || [];
}

Chain.prototype.push = function push(cb){
    this.items.push(cb);
    return this;
}

Chain.prototype.run = function run(end){
    var self = this,
        end = end || function(){}

    var next = function(i){
        return function(){
            if (i < self.items.length){
                self.items[i++](next(i), end);
            }
            else
            {
                end();
            }
        }
    };
    next(0)();
}

module.exports = Chain;