// Copyright (C) 2009 The Android Open Source Project
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package com.google.gimd.javaglue;

import java.util.Collection;

import scala.Function1;
import scala.runtime.AbstractFunction1;
import scala.Function1$class;

public class ScalaUtil {

  public static <T,R> Function1<T,R> fun1(final JFunction1<T,R> f) {
    return new AbstractFunction1<T,R>() {

      @Override
      public R apply(T x) {
        try {
          return f.apply(x);
        } catch (Throwable e) {
          throw new RuntimeException(e);
        }
      }

    };
  }

  @SuppressWarnings("unchecked")
  public static <T> scala.collection.immutable.List<T> toScalaList(Collection<T> xs) {
    //Java sucks as there is no way to create a generic Array so casting is needed
    return scala.collection.immutable.List$.MODULE$.fromArray(xs.toArray( (T[])new Object[xs.size()] ));
  }

  public static <T,U> scala.Tuple2<T,U> tuple2(T x, U y) {
    return new scala.Tuple2<T,U>(x, y);
  }

}
