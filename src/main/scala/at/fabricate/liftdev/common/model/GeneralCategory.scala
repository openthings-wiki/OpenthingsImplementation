package at.fabricate.liftdev.common
package model

import net.liftweb.mapper.IdPK

trait GeneralCategoryMeta[ModelType <: (GeneralCategory[ModelType]) ] extends BaseMetaEntity[ModelType] with BaseMetaEntityWithTitleAndDescription[ModelType] {
	self: ModelType => 
  
}
  

trait GeneralCategory[T <: GeneralCategory[T] ] extends BaseEntity[T] with BaseEntityWithTitleAndDescription[T] with IdPK with EnsureUniqueTextFields[T] {
  	self: T =>
  	  
  	 override type TheUniqueTextType = TheTranslation

	//override def theUniqueFields = List(TheTranslationMeta.title)
  	 override val titleValidations = List(ensureFieldIsUnique(TheTranslationMeta.title) _ )
}