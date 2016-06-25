(function (ko, resumeData) {
  'use strict';

  var MONTHS,
      formatMonth,
      formatDay,
      getUpdatedResume,
      ResumeViewModel;

  MONTHS = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  ];

  formatMonth = function(month) {
    return MONTHS[month];
  };

  formatDay = function(day) {
    if (day == 11) {
      return day + "th";
    } else if (day === 12) {
      return day + "th";
    } else if (day === 13) {
      return day + "th";
    }

    var d = day % 10;
    if (d === 1) {
      return day + "st";
    } else if (d === 2) {
      return day + "nd";
    } else if (d === 3) {
      return day + "rd";
    } else {
      return day + "th";
    };
  };

  getUpdatedResume = function (id) {
    var toRet = resumeData.filter(function (resume) {
      return resume.id === id;
    });
    return toRet[0];
  };

  ResumeViewModel = function () {
    var self = this;

    self.resume = ko.observable();

    self.refreshResume = function() {
      self.resume(getUpdatedResume(123));
    };

    self.refreshResume();

    self.visibleReferences = ko.computed(function() {
      console.log('resume is', self.resume());
      var toRet = self.resume().references.filter(function(x) {
        return x.visible;
      });
      return toRet;
    });

    self.formatContactAttr = function(type, value) {
      if (type === "Email") {
        return "mailto:" + value;
      } else if (type === "Cell") { //TODO: Put in more types here (and use a better way to do it).
        return "tel:" + value;
      } else { //TODO: Maybe work a "URL" type, and display a "name" or something instead.
        return value;
      }
    };

    self.formatContactValue = function(type, value) {
      return value;
    };

    self.formatDate = function(date) {
      if (date === "present") {
        return date;
      } else {
        var d = new Date(Date.parse(date));
        return formatMonth(d.getMonth()) + " " + formatDay(d.getDate()) + ", " + d.getFullYear();
      }
    };

    self.fullName = function(name) {
      return name.given + (name.middle ? " " + name.middle + " " : " ") + name.surname;
    };

  };

  ko.applyBindings(new ResumeViewModel());

}(window.ko, window.resumeData));
