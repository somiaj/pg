#########################################################################
#
#  Implements base class for data list in Context objects.
#
package Value::Context::Data;
use strict;
use Scalar::Util;

sub new {
  my $self = shift; my $class = ref($self) || $self;
  my $parent = shift;
  my $data = bless {
    context => $parent,     # parent context
    dataName => {},         # name of data storage in context hash
    tokens => {},           # hash of id => type specifications that will be made into a pattern
    patterns => {},         # hash of pattern => [precedence,type] specification for extra patterns
    tokenType => {},        # type of Parser token for these patterns
    namePattern => '',      # pattern for allowed names for new items
    name => '', Name => '', # lower- and upper-case names for the class of items
    allowAlias => 1,        # allow entries to use alias property to point to another
  }, $class;
  $data->weaken;
  $data->init();
  $parent->{$data->{dataName}} = {};
  push @{$parent->{data}{objects}},"_$data->{dataName}";
  $data->add(@_);
  return $data;
}

#
#  Implemented in sub-classes
#
sub init {}
sub create {shift; shift}
sub uncreate {shift; shift}

#
#  Copy the context data
#
sub copy {
  my $self = shift; my $orig = shift;
  my $data = $orig->{context}->{$orig->{dataName}};
  my $copy = $self->{context}->{$self->{dataName}};
  foreach my $name (keys %{$data}) {
    if (ref($data->{$name}) eq 'ARRAY') {
      $copy->{$name} = [@{$data->{$name}}];
    } elsif (ref($data->{$name}) eq 'HASH') {
      $copy->{$name} = {%{$data->{$name}}};
    } else {
      $copy->{$name} = $data->{$name};
    }
  }
  $self->{tokens} = {%{$orig->{tokens}}};
  foreach my $p (keys %{$orig->{patterns}}) {
    $self->{patterns}{$p} =
      (ref($orig->{patterns}{$p}) ? [@{$orig->{patterns}{$p}}] : $orig->{patterns}{$p});
  }
}

#
#  Make context pointer a weak pointer (avoids reference loops)
#
sub weaken {Scalar::Util::weaken((shift)->{context})}

#
#  Update the context patterns
#
sub update {(shift)->{context}->update}

#
#  Add the token to the tokens list, and add any alternative forms
#
sub addToken {
  my $self = shift; my $token = shift;
  my $def = $self->{context}{$self->{dataName}}{$token};
  unless ($def->{hidden}) {
    if (defined $def->{patternPrecedence}) {
      $self->{patterns}{Parser::Context::protectRegexp($token)} = [$def->{patternPrecedence}, $self->{tokenType}];
    } else {
      $self->{tokens}{$token} = $self->{tokenType};
    }
    $self->addAlternatives($token,$def->{alternatives});
  }
}
sub addAlternatives {
  my $self = shift; my $token = shift; my $alternatives = shift || [];
  foreach my $alt (@$alternatives) {
    Value::Error("Illegal %s name '%s'",$self->{name},$alt) unless $alt =~ m/^$self->{namePattern}$/;
    $self->{tokens}{$alt} = [$self->{tokenType},$token];
  }
}

#
#  Remove the token from the tokens list, and any alternative forms
#
sub removeToken {
  my $self = shift; my $token = shift;
  delete $self->{tokens}{$token};
  $self->removeAlternatives($token,$self->{context}{$self->{dataName}}{$token}{alternatives});
}
sub removeAlternatives {
  my $self = shift; my $token = shift; my $alternatives = shift || [];
  foreach my $alt (@$alternatives) {delete  $self->{tokens}{$alt}}
}

#
#  Add one or more new items to the list
#
sub add {
  my $self = shift; my %D = (@_); return if scalar(@_) == 0;
  my $data = $self->{context}{$self->{dataName}};
  foreach my $x (keys %D) {
    Value::Error("Illegal %s name '%s'",$self->{name},$x) unless $x =~ m/^$self->{namePattern}$/;
    warn "$self->{Name} '$x' already exists" if defined($data->{$x}) && ! $data->{$x}->{autoGen};
    if ($data->{$x}) {
      delete $self->{tokens}{$x};
      delete $self->{patterns}{Parser::Context::protectRegexp($x)};
    }
    $data->{$x} = $self->create($D{$x});
    $self->addToken($x);
    if (ref($data->{$x}) eq 'HASH' && $data->{$x}{alias}) {
      Value::Error("Alias not allowed with %s objects",$self->{name}) unless $self->{allowAlias};
      my $alias = $data->{$x}{alias};
      Value::Error("Alias '%s' doesn't exist for %s '%s'",$alias,$self->{name},$x)
          if !(defined($data->{$alias}) || defined($D{$alias}));
    } elsif (ref($data->{$x}) eq 'HASH' && $data->{$x}{formatGreek}) {
      $self->formatGreekLetters($x);
    }
  }
  $self->update;
}

#
#  Remove one or more items
#
sub remove {
  my $self = shift;
  my $data = $self->{context}{$self->{dataName}};
  foreach my $x (@_) {
    warn "$self->{Name} '$x' doesn't exist" unless defined($data->{$x});
    $self->removeToken($x);
    delete $data->{$x};
  }
  $self->update;
}

#
#  Replace an item with a new definition
#
sub replace {
  my $self = shift; my %list = (@_);
  $self->remove(keys %list);
  $self->add(@_);
}

#
#  Clear all items
#
sub clear {
  my $self = shift;
  $self->{context}{$self->{dataName}} = {};
  $self->{tokens} = {};
  $self->update;
}

#
#  Make the data be only these items
#
sub are {
  my $self = shift;
  $self->clear;
  $self->add(@_);
}

#
#  Make one or more items become undefined, but still recognized.
#  (Implemented in the sub-classes.)
#
sub undefine {my $self = shift; $self->remove(@_)}

#
#  Redefine items from the default context, or a given one
#
sub redefine {
  my $self = shift; 
  my $X = shift;
  my %options = @_; 
  warn "error in options must be an even number of entries:  ", join(" ",%options), caller(1) if @_%2;  # require even number
  %options =  (using => undef, from => "Full", %options) ;
  my $Y = $options{using}; my $from = $options{from};
  $from = $Parser::Context::Default::context{$from} unless ref($from);
  $Y = $X if !defined($Y) && !ref($X);
  $X = [$X] unless ref($X) eq 'ARRAY';
  my @data = (); my @remove = ();
  foreach my $x (@{$X}) {
    my $y = defined($Y)? $Y: $x;
    Value::Error("No definition for %s '%s' in the given context",$self->{name},$y)
      unless $from->{$self->{dataName}}{$y};
    push(@remove,$x) if $self->get($x);
    push(@data,$x => $self->uncreate($from->{$self->{dataName}}{$y}));
  }
  $self->remove(@remove);
  $self->add(@data);
}


#
#  Get hash for an item
#
sub get {
  my $self = shift; my $x = shift;
  return $self->{context}{$self->{dataName}}{$x};
}

#
#  Set flags for one or more items
#
sub set {
  my $self = shift; my %D = (@_);
  my $data = $self->{context}{$self->{dataName}};
  my $update = 0;
  foreach my $x (keys(%D)) {
    my $xref = $data->{$x};
    if (defined($xref) && ref($xref) eq 'HASH') {
      foreach my $id (keys %{$D{$x}}) {
        $xref->{$id} = $D{$x}{$id};
        if ($id eq 'alternatives' && !$D{$x}{hidden}) {
          $self->addAlternatives($x, $D{$x}{$id});
          $update = 1;
        }
        if ($id eq 'patternPrecedence') {
          delete $self->{tokens}{$x};
          $self->addToken($x);
          $update = 1;
        }
      }
    } else {
      $data->{$x} = $self->create($D{$x});
      $self->addToken($x);
      $update = 1;
    }
  };
  $self->update if $update;
}

#
#  Follow aliases to get final definition
#
sub resolveDef {
  my $self = shift;
  ($self->resolve(@_))[1];
}

#
#  Follow aliases to get final name and definition
#
sub resolve {
  my $self = shift; my $name = shift;
  my $data = $self->{context}{$self->{dataName}};
  my $def = $data->{$name};
  $name = $def->{alias}, $def = $data->{$name} while defined($def) && $def->{alias};
  return ($name, $def);
}

#
#  Get the names of all items
#
sub names {
  my $self = shift;
  return sort(keys %{$self->{context}{$self->{dataName}}});
}

#
#  Get the complete data hash
#
sub all {
  my $self = shift;
  $self->{context}{$self->{dataName}};
}

#
#  Add TeX and unicode alternatives to Greek letters
#
sub formatGreekLetters {
  my $self = shift; my $name = shift;
  my $data = $self->{context}{$self->{dataName}};
  my $def = $data->{$name};

  # Using greek letters defined by the AMS
  # http://www.ams.org/STIX/ion/stixsig03.html
  my $greekLetters = {
    'Gamma'   => { TeX => '\Gamma ',      alternatives => ["\x{0393}"] },
    'Delta'   => { TeX => '\Delta ',      alternatives => ["\x{0394}"] },
    'Theta'   => { TeX => '\Theta ',      alternatives => ["\x{0398}"] },
    'Lambda'  => { TeX => '\Lambda ',     alternatives => ["\x{039B}"] },
    'Xi'      => { TeX => '\Xi ',         alternatives => ["\x{039E}"] },
    'Pi'      => { TeX => '\Pi ',         alternatives => ["\x{03A0}"] },
    'Sigma'   => { TeX => '\Sigma ',      alternatives => ["\x{03A3}"] },
    'Phi'     => { TeX => '\Phi ',        alternatives => ["\x{03A6}"] },
    'Psi'     => { TeX => '\Psi ',        alternatives => ["\x{03A8}"] },
    'Omega'   => { TeX => '\Omega ',      alternatives => ["\x{03A9}"] },

    'alpha'   => { TeX => '\alpha ',      alternatives => ["\x{03B1}"] },
    'beta'    => { TeX => '\beta ',       alternatives => ["\x{03B2}"] },
    'gamma'   => { TeX => '\gamma ',      alternatives => ["\x{03B3}"] },
    'delta'   => { TeX => '\delta '   ,   alternatives => ["\x{03B4}"] },
    'epsilon' => { TeX => '\varepsilon ', alternatives => ["\x{03B5}"] },
    'zeta'    => { TeX => '\zeta ',       alternatives => ["\x{03B6}"] },
    'eta'     => { TeX => '\eta ',        alternatives => ["\x{03B7}"] },
    'theta'   => { TeX => '\theta ',      alternatives => ["\x{03B8}"] },
    'iota'    => { TeX => '\iota ',       alternatives => ["\x{03B9}"] },
    'kappa'   => { TeX => '\kappa ',      alternatives => ["\x{03BA}"] },
    'lambda'  => { TeX => '\lambda ',     alternatives => ["\x{03BB}"] },
    'mu'      => { TeX => '\mu ',         alternatives => ["\x{03BC}"] },
    'nu'      => { TeX => '\nu ',         alternatives => ["\x{03BD}"] },
    'xi'      => { TeX => '\xi ',         alternatives => ["\x{03BE}"] },
    'pi'      => { TeX => '\pi ',         alternatives => ["\x{03C0}"] },
    'rho'     => { TeX => '\rho ',        alternatives => ["\x{03C1}"] },
    'sigma'   => { TeX => '\sigma ',      alternatives => ["\x{03C3}"] },
    'tau'     => { TeX => '\tau ',        alternatives => ["\x{03C4}"] },
    'upsilon' => { TeX => '\upsilon ',    alternatives => ["\x{03C5}"] },
    'phi'     => { TeX => '\phi ',        alternatives => ["\x{03C6}"] },
    'chi'     => { TeX => '\chi ',        alternatives => ["\x{03C7}"] },
    'psi'     => { TeX => '\psi ',        alternatives => ["\x{03C8}"] },
    'omega'   => { TeX => '\omega ',      alternatives => ["\x{03C9}"] },
  };

  if ($greekLetters->{$name}) {
    $def->{TeX} = $greekLetters->{$name}->{TeX} if ! $def->{TeX};
    foreach my $alt (@{$greekLetters->{$name}{alternatives}}) {
      if (! defined($data->{$alt})) {
        # Currently only used for variables and constants.
        if ($self->{dataName} eq 'variables') {
          $self->add($alt => [$def->{type}, alias => $name]);
        } elsif ($self->{dataName} eq 'constants') {
          $self->add($alt => { alias => $name });
        }
        $data->{$alt}{autoGen} = 1 if $data->{$alt}; # Allow overwrite without warnings.
      }
    }
  }
}

#########################################################################
#
#  Load the subclasses.
#

END {
  use Value::Context::Flags;
  use Value::Context::Lists;
  use Value::Context::Diagnostics;
}

#########################################################################

1;
