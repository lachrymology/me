Function.prototype.method = function(name, func) {
  this.prototype[name] = func;
  return this;
};

Object.method('superior', function(name) {
  var that = this, method = that[name];
  return function() { return method.apply(that, arguments); };
});


var person = function(spec) {
  var that = {};

  that.getName = function() {
    return spec.name;
  };
  return that;
};

/*
var me = person({'name' : 'Fogus'});
me.getName()
//=> 'Fogus'
*/

var man = function(spec) {
  var that = person(spec);
  var super_getName = that.superior('getName');

  that.getName = function() {
    return "Mr. " + super_getName();
  };

  return that;
};

/*
var me = man({'name' : 'Fogus'});
me.getName()
//=> 'Mr. Fogus'
*/

/*******************************************/

var inherits = function(child, parent) {
  function tmp() {};
  tmp.prototype = parent.prototype;
  child._super = parent.prototype;
  child.prototype = new tmp();
  child.prototype.constructor = child;
};

Person = function(name) {
  this._name = name;
};

Person.prototype.getName = function() {
  return this._name;
};

/*
var me = new Person('Fogus');
me.getName();
//=> 'Fogus'
*/

Man = function(name) {
  Person.call(this, name);
};

inherits(Man, Person);

Man.prototype.getName = function() {
  return "Mr. " + Man._super.getName.call(this);
};

/*
var me = new Man('Fogus');
me.getName();
//=> 'Mr. Fogus'
*/

/*
me._name
//=> 'Fogus'
 */




function stuffToArray() {
  return Array.prototype.slice.call(arguments);
}

function addFirstTwo() {
  var args = Array.prototype.slice.call(arguments);
  return cljs.first(args) + cljs.second(args);
}