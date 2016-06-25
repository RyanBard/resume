var resumeData = [
  {
    "id" : 123,
    "purpose" : "To obtain a software developer position and grow in an interesting language or field.",
    "interests" : [
      "C",
      "C++",
      "C#",
      "JavaScript",
      "Objective-C",
      "Ruby"
    ],
    "name" : {
      "surname" : "Bard",
      "given" : "John",
      "middle" : "Ryan"
    },
    "addresses" : [
      {
        "street" : "2306 Robin Lane",
        "city" : "Smyrna",
        "state" : "GA",
        "zip" : "30080"
      }
    ],
    "contacts" : [
      {
        "type" : "Cell",
        "value" : "678-438-9941"
      },
      {
        "type" : "Email",
        "value" : "john.ryan.bard@gmail.com"
      }
    ],
    "social" : [
      {
        "type" : "LinkedIn",
        "value" : "http://www.linkedin.com/pub/john-bard/12/b5a/551"
      },
      {
        "type" : "GitHub",
        "value" : "https://github.com/RyanBard"
      }
    ],
    "work_experience" : [
      {
        "date_range" : {
          "start" : "2013-02-04",
          "end" : null
        },
        "company" : "Ericsson",
        "title" : "Senior Software Engineer",
        "description" : "Developed backend software that supports many aspects of Video On Demand.  This included rentals, bookmarks, setup and teardown of HTTP and RTSP sessions, generating URLs for CDNs, applying concurrency restrictions, authenticing requests, saving statistics, and issuing alerts.",
        "responsibilities" : [
        ],
        "achievements" : [
          "Scripts and writeups for consistent dev deployment steps",
          "Automated smoke/integration tests",
          "Helped build tools for QA to automate tests (sdk wrapper for auth testing, tweaked the RTSP plugin, created a CDN simulator)",
          "Fixed the CI (adjusted nightly builds, turned on unit test running)",
          "Cleaned up some modules"
        ],
        "technologies" : [
          "Erlang R15 & R16",
          "Rebar",
          "MochiWeb",
          "Cowboy",
          "WebMachine",
          "Meck",
          "Emongo",
          "REST",
          "RTSP",
          "JSON",
          "NoSQl (MongoDB)",
          "RabbitMQ",
          "Ruby 1.9",
          "Ruby on Rails 3.2",
          "RVM",
          "Bundler",
          "Rake",
          "Rspec",
          "Decent Exposure",
          "Mongoid 2",
          "Git",
          "Bash",
          "Yum",
          "Jenkins",
          "Curl",
          "Jmeter",
          "Java 1.7"
        ],
        "soft_skills" : [
          "Gitorious",
          "GitHub",
          "VersionOne"
        ]
      },
      {
        "date_range" : {
          "start" : "2012-09-04",
          "end" : "2013-01-18"
        },
        "company" : "Altisource",
        "title" : "Senior Software Engineer",
        "description" : "Developed software involved in migrating backlog data and synchronizing live updates of that data from a legacy database to the new products' MySQL database.",
        "responsibilities" : [
        ],
        "achievements" : [
          "Added the first true unit tests",
          "Cleaned up integration tests",
          "Optimized and cleaned up the build",
          "Got the project ready for CI (Bamboo)",
          "Corrected anti-patterns rampant throughout the project (and a few bugs)",
          "Cleaned up tons of compiler warnings",
          "Changed the process of developers running locally (and running integration tests) against a shared database to everyone running their own local instance of MySQL and using that instead"
        ],
        "technologies" : [
          "Java 1.7",
          "Mockito",
          "Mybatis",
          "Spring Integration",
          "Spring REST Client",
          "Groovy",
          "JSON",
          "Apache Maven 3",
          "MySQL",
          "Git",
          "RPM",
          "Chef",
          "RabbitMQ/AMQP",
          "Yammer Metrics"
        ],
        "soft_skills" : [
          "Bamboo",
          "Fisheye",
          "Confluence",
          "JIRA"
        ]
      },
      {
        "date_range" : {
          "start" : "2008-02-12",
          "end" : "2012-08-31"
        },
        "company" : "MModal (formerly MedQuist)",
        "title" : "Software Architect",
        "description" : "Architected, designed, and developed software for medical transcription services. Used various technologies including EJB, Hibernate, Spring, etc.",
        "responsibilities" : [
        ],
        "achievements" : [
          "Set up Splunk to index logs and email alerts for errors in dev, QA, and PT",
          "Refactored many pieces of legacy JDBC and stored procedure code to use Spring JdbcTemplate and JPA Entities",
          "Wrote a series of DB scripts to create a schema from scratch so I could have a repeatable fresh environment separate from our shared dev environment (there was only the shared dev environment to test on for this project)",
          "With Talend Open Studio, I took a month to rewrite an archive and purge solution that took one of our DBAs a year to develop in PL/SQL (and didn't solve all of the requirements)"
        ],
        "technologies" : [
          "Java 1.5 & 1.6",
          "Apache Maven 2",
          "JMS",
          "Apache Struts1",
          "PMD",
          "JBoss 4.0.5, 4.2, 4.3, & 5.1",
          "Tiles",
          "Cobertura",
          "JMX MBeans",
          "JavaScript",
          "JUnit 3 & 4",
          "PL/SQL",
          "Hibernate/JPA",
          "EasyMock",
          "SQL (Oracle)",
          "JDBC",
          "Selenium",
          "Talend Open Studio",
          "slf4j",
          "Logging",
          "SoapUI",
          "JProfiler",
          "Jaxb",
          "AspectJ",
          "Oracle Coherence (as a 2nd level cache)",
          "Groovy",
          "Velocity Templates",
          "EJB2 (JTA, Stateless Session, MDBs)",
          "Sed/Awk",
          "Bash Scripting",
          "EJB3 (JTA, Stateless Session, MDBs, Interceptors)",
          "Splunk 3 & 4",
          "Batch Scripting",
          "J2EE (JSP, Custom Taglibs, Servlets)",
          "XML (DOM, XSLT, Xpath, Xquery)",
          "OSB (Oracle Service Bus - Oracle's ESB)",
          "Python/IronPython/Jython",
          "Spring Framework (core, tx, ws, jdbc, webmvc)",
          "C# (consumed our webservices by writing test apps in C#)"
        ],
        "soft_skills" : [
        ]
      },
      {
        "date_range" : {
          "start" : "2007-08-01",
          "end" : "2008-02-11"
        },
        "company" : "Guru Software Services",
        "title" : "Software Engineer - Lead Developer",
        "description" : "I was the Lead Developer in the U.S. I interfaced with customers and managed an outsource team in India. I primarily developed in J2EE technologies with Eclipse. I used Spring Framework for dependency injection, Apache Commons for logging, and Hibernate for Object-Relational Mapping. I created and maintained webapps with JSP that were tested in Apache Tomcat and wrote the Ant build files for deployment (in both Windows and Linux environment). I also researched the control of scanners with JTwain and the re-writing of a TWAIN wrapper for Java.",
        "responsibilities" : [
          "Gather requirements from customer and fine tuned these requirements in daily/weekly meetings",
          "Design interfaces and create associated documentation (screen shots & storyboards)",
          "Oversee the functionality & development of the user interface",
          "Design the implementation and create the design documents (UML)",
          "Come up with the initial time bids for projects",
          "Responsible for communicating the requirements to the outsource team in India",
          "Responsible for code reviews (and intervene when problems arise)",
          "Responsible for Q/A testing in both Windows and Linux environment",
          "Responsible for customer acceptance testing and any modifications to the project"
        ],
        "achievements" : [
        ],
        "technologies" : [
          "Java 1.4",
          "J2EE",
          "Apache Commons Logging (log4j)",
          "JTwain/TWAIN",
          "Spring Framework",
          "Apache Ant",
          "JNI",
          "Hibernate",
          "Apache Tomcat",
          "HTML",
          "CSS",
          "JSP"
        ],
        "soft_skills" : [
        ]
      },
      {
        "date_range" : {
          "start" : "2007-05-21",
          "end" : "2007-07-31"
        },
        "company" : "Guru Software Services (via Georgia Tech)",
        "title" : "Software Engineer - Team Lead",
        "description" : "I was the Team Lead in the development of the Rates Management Module of the Logi-soft software. We created a JSP webapp for managing shipping rates and calculating estimates for trucking costs. We used Hibernate for ORM and tested in Tomcat. In addition to developing the JSP pages and Hibernate code, I personally setup and managed a CVS repository as well as wrote the Ant build file and handled the deployment and delivery of the module to Guru Software Services.",
        "responsibilities" : [
        ],
        "achievements" : [
        ],
        "technologies" : [
          "Java 1.4",
          "J2EE",
          "Apache Ant",
          "Hibernate",
          "Apache Tomcat",
          "HTML",
          "CSS",
          "JSP"
        ],
        "soft_skills" : [
        ]
      }
    ],
    "hobbies" : {
      "description" : "Almost all of my hobbies are centered around computers and technology. If the Zombie Apocalypse ever comes, I'm in trouble.",
      "hobbies" : [
        "JavaScript",
        "KnockoutJS",
        "AngularJS",
        "NodeJS (Express, Mongoose)",
        "Groovy",
        "Grails",
        "Python",
        "Perl",
        "Ruby",
        "Rails",
        "Clojure",
        "YAWS",
        "C",
        "C++",
        "Qt",
        "GTK+",
        "Arduino",
        "Objective-C",
        "C#",
        "Smalltalk",
        "MongoDB",
        "Cassandra",
        "Neo4J",
        "Hadoop",
        "ANTLR",
        "Lex & Yacc",
        "Debian",
        "FreeBSD",
        "Lua",
        "WebRTC",
        "HTML5 (Canvas, LocalStorage, WebSocket, WebWorker)",
        "Elixir",
        "Homebrewing (beer, mead, and cider)"
      ]
    },
    "education" : [
      {
        "degree" : "B.S. in Computer Science",
        "school_name" : "Georgia Institute of Technology",
        "graduation_date" : "2007-12",
        "gpa" : 3.05,
        "major_gpa" : 3.44
      }
    ],
    "references" : [
      {
        "visible" : true,
        "name" : {
          "surname" : "Burdell",
          "middle" : "P",
          "given" : "George"
        },
        "contacts" : [
          {
            "type" : "Email",
            "value" : "goerge.p.burdell@gatech.edu"
          }
        ]
      },
      {
        "visible" : true,
        "name" : {
          "surname" : "Soze",
          "given" : "Keyser"
        },
        "contacts" : [
          {
            "type" : "Email",
            "value" : "ksoze@nowhere.com"
          }
        ]
      }
    ]
  }
];
