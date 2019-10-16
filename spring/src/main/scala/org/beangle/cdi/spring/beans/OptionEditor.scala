package org.beangle.cdi.spring.beans

import java.beans.PropertyEditorSupport

class OptionEditor extends PropertyEditorSupport {

  override def setAsText(text: String): Unit = {
    setValue(text)
  }

  override def setValue(value: AnyRef): Unit = {
    super.setValue(Option(value))
  }
}
