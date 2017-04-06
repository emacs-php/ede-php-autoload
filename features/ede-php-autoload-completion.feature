Feature: Class name completion

  Scenario: Complete PSR-0 namespaces
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "Psr0" should be:
      | Psr0Ns | Psr0Split\Ns1 | Psr0Split\Ns2  |

  Scenario: Complete PSR-0 namespace with slashes
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "Psr0Ns\T" should be:
      | TheClass  |

  Scenario: Complete PSR-0 namespace with underscores
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "Psr0Ns_T" should be:
      | Psr0Ns_TheClass  |

  Scenario: Complete PSR-4 namespaces
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "Psr4" should be:
      | Psr4Ns | Psr4Split\Ns1 | Psr4Split\Ns2 |
    And completions for query "Psr4Ns\T" should be:
      | TheClass |

  Scenario: Complete PSR-4 multi-dir namespaces
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "MultiDirNs\T" should be:
      | TheClass1 | TheClass2 |

  Scenario: Complete classmap namespaces
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "ClassMapNs\" should be:
      | ClassMapNs\MyClass |

  Scenario: Complete non-existing dir
    Given I visit "src/main.php" in project "without-composer"
    Then completions for query "NonExisting\" should be nil
