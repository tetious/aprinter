/*
 * Copyright (c) 2016 Ambroz Bizjak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef APRINTER_IPSTACK_STRUCT_H
#define APRINTER_IPSTACK_STRUCT_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <aprinter/meta/TypeListUtils.h>
#include <aprinter/meta/BasicMetaUtils.h>
#include <aprinter/base/BinaryTools.h>

#include <aprinter/BeginNamespace.h>

template <typename Type, typename Dummy=void>
struct StructTypeHandler;

#define APRINTER_STRUCT_REGISTER_TYPE(Type, TypeHandler) \
template <> \
struct StructTypeHandler<Type, void> { \
    using Handler = TypeHandler; \
};

template <typename TType>
struct StructField {
    using Type = TType;
};

/**
 * Base class for protocol structure definitions.
 * 
 * Notable features of the system are:
 * - Automatic endianness handling (big endian encoding is used).
 *   That is, the user always interacts with logical values while
 *   the framework manages the byte-level representaion.
 * - Can reference structures in existing memory (no need for pointer
 *   casts violatign strict aliasing).
 * - Support for nested structures.
 * - Ability to define custom field types.
 * 
 * Structures should be defined through the APRINTER_TSTRUCT macro which
 * results in a struct type inheriting StructBase. Example:
 * 
 * \code
 * APRINTER_TSTRUCT(MyHeader,
 *     (FieldA, uint32_t)
 *     (FieldB, uint64_t)
 * )
 * \endcode
 * 
 * Each field specified will result in a type within this structure that
 * is used as an identifier for the field (e.g. MyHeader::FieldA).
 * 
 * There will be three classes within the main structure providing
 * different ways to work with structure data:
 * - Val: contains structure data as char[].
 * - Ref: references structure data using char *.
 * - ConstRef: references structure data using char const *.
 * 
 * Note that the structure type itself (MyHeader) has no runtime use, it
 * is a zero-sized structure.
 * 
 * Note, internally APRINTER_TSTRUCT will expand to something like this:
 * 
 * struct MyHeader : public StructBase\<MyHeader\> {
 *      struct FieldA : public StructField\<uint32_t\>;
 *      struct FieldB : public StructField\<uint64_t\>;
 *      using StructFields = MakeTypeList\<FieldA, FieldB\>;
 * };
 */
template <typename TStructType>
class StructBase {
private:
    using StructType = TStructType;
    
    template <typename This=StructBase>
    using Fields = typename This::StructType::StructFields;
    
    template <int FieldIndex, typename Dummy=void>
    struct FieldInfo;
    
    template <typename Dummy>
    struct FieldInfo<-1, Dummy> {
        static size_t const StructSize = 0;
    };
    
    template <int FieldIndex, typename Dummy>
    struct FieldInfo {
        using PrevFieldInfo = FieldInfo<FieldIndex-1, void>;
        using Field = TypeListGet<Fields<>, FieldIndex>;
        
        using Handler = typename StructTypeHandler<typename Field::Type>::Handler;
        using ValType = typename Handler::ValType;
        static size_t const FieldOffset = PrevFieldInfo::StructSize;
        static size_t const FieldSize = Handler::FieldSize;
        static size_t const StructSize = FieldOffset + FieldSize;
    };
    
    template <typename Field, typename This=StructBase>
    using GetFieldInfo = FieldInfo<TypeListIndex<Fields<This>, Field>::Value, void>;
    
    template <typename This=StructBase>
    using LastFieldInfo = FieldInfo<TypeListLength<Fields<This>>::Value-1, void>;
    
public:
    class Ref;
    class ConstRef;
    class Val;
    
    /**
     * Gets the value type of a specific field.
     * 
     * Example: ValType\<MyHeader::FieldA\> is uint32_t.
     * 
     * @tparam Field field identifier
     */
    template <typename Field>
    using ValType = typename StructTypeHandler<typename Field::Type>::Handler::ValType;
    
    /**
     * Gets the reference type of a specific field.
     * Support for this depends on the type handler (e.g. nested structures).
     */
    template <typename Field>
    using RefType = typename StructTypeHandler<typename Field::Type>::Handler::RefType;
    
    /**
     * Gets the const-reference type of a specific field.
     * Support for this depends on the type handler (e.g. nested structures).
     */
    template <typename Field>
    using ConstRefType = typename StructTypeHandler<typename Field::Type>::Handler::ConstRefType;
    
    /**
     * Returns the size of the structure.
     */
    inline static constexpr size_t Size () 
    {
        return LastFieldInfo<>::StructSize;
    }
    
    /**
     * Reads a field.
     * 
     * @tparam Field field identifier
     * @param data pointer to the start of a structure
     * @return field value that was read
     */
    template <typename Field>
    inline static ValType<Field> get (char const *data, Field)
    {
        using Info = GetFieldInfo<Field>;
        return Info::Handler::get(data + Info::FieldOffset);
    }
    
    /**
     * Writes a field.
     * 
     * @tparam Field field identifier
     * @param data pointer to the start of a structure
     * @param value field value to set
     */
    template <typename Field>
    inline static void set (char *data, Field, ValType<Field> value)
    {
        using Info = GetFieldInfo<Field>;
        Info::Handler::set(data + Info::FieldOffset, value);
    }
    
    /**
     * Returns a reference to a field.
     * Support for this depends on the type handler (e.g. nested structures).
     */
    template <typename Field>
    inline static RefType<Field> ref (char *data, Field)
    {
        using Info = GetFieldInfo<Field>;
        return Info::Handler::ref(data + Info::FieldOffset);
    }
    
    /**
     * Returns a const reference to a field.
     * Support for this depends on the type handler (e.g. nested structures).
     */
    template <typename Field>
    inline static ConstRefType<Field> ref (char const *data, Field)
    {
        using Info = GetFieldInfo<Field>;
        return Info::Handler::const_ref(data + Info::FieldOffset);
    }
    
    /**
     * Returns a Ref class referencing the specified memory.
     * 
     * @param data pointer to the start of a structure
     */
    inline static Ref MakeRef (char *data)
    {
        return Ref{data};
    }
    
    /**
     * Returns a ConstRef class referencing the specified memory.
     * 
     * @param data pointer to the start of a structure
     */
    inline static ConstRef MakeRef (char const *data)
    {
        return ConstRef{data};
    }
    
    /**
     * Reads a structure from the specified memory location and
     * returns a Val class containing the structure data.
     * 
     * @param data pointer to the start of a structure
     * @return a Val class initialized with a copy of the data
     */
    inline static Val MakeVal (char const *data)
    {
        Val val;
        memcpy(val.data, data, Size());
        return val;
    }
    
    /**
     * Class which contains structure data.
     * These can be created using StructBase::MakeVal or from
     * the Val conversion operators in Ref and ConstRef.
     */
    class Val {
    public:
        using Struct = StructType;
        
        /**
         * Reads a field.
         * @see StructBase::get
         */
        template <typename Field>
        inline ValType<Field> get (Field) const
        {
            return StructBase::get(data, Field());
        }
        
        /**
         * Writes a field.
         * @see StructBase::set
         */
        template <typename Field>
        inline void set (Field, ValType<Field> value)
        {
            StructBase::set(data, Field(), value);
        }
        
        /**
         * Returns a reference to a field.
         * @see StructBase::ref
         */
        template <typename Field>
        inline RefType<Field> ref (Field)
        {
            return StructBase::ref(data, Field());
        }
        
        /**
         * Returns a const reference to a field.
         * @see StructBase::ref
         */
        template <typename Field>
        inline ConstRefType<Field> ref (Field) const
        {
            return StructBase::ref(data, Field());
        }
        
        /**
         * Returns a Ref referencing this Val.
         */
        inline operator Ref ()
        {
            return Ref{data};
        }
        
        /**
         * Returns a ConstRef referencing this Val.
         */
        inline operator ConstRef () const
        {
            return ConstRef{data};
        }
        
    public:
        /**
         * The data array.
         */
        char data[LastFieldInfo<>::StructSize];
    };
    
    /**
     * Structure access class referencing external data via
     * char *.
     * Can be initialized via StructBase::MakeRef or Ref{data}.
     */
    class Ref {
    public:
        using Struct = StructType;
        
        /**
         * Reads a field.
         * @see StructBase::get
         */
        template <typename Field>
        inline ValType<Field> get (Field) const
        {
            return StructBase::get(data, Field());
        }
        
        /**
         * Writes a field.
         * @see StructBase::set
         */
        template <typename Field>
        inline void set (Field, ValType<Field> value) const
        {
            StructBase::set(data, Field(), value);
        }
        
        /**
         * Returns a reference to a field.
         * @see StructBase::ref
         */
        template <typename Field>
        inline RefType<Field> ref (Field) const
        {
            return StructBase::ref(data, Field());
        }
        
        /**
         * Returns a ConstRef referencing the same memory.
         */
        inline operator ConstRef () const
        {
            return ConstRef{data};
        }
        
        /**
         * Reads and returns the current structure data as a Val.
         */
        inline operator Val () const
        {
            return MakeVal(data);
        }
        
        /**
         * Copies the structure referenced by a ConstRef
         * over the structure referenced by this Ref.
         * Note: uses memcpy, so don't use with self.
         */
        inline void load (ConstRef src) const
        {
            memcpy(data, src.data, Size());
        }
        
    public:
        char *data;
    };
    
    /**
     * Structure access class referencing external data via
     * char const *.
     * Can be initialized via StructBase::MakeRef or ConstRef{data}.
     */
    class ConstRef {
    public:
        using Struct = StructType;
        
        /**
         * Reads a field.
         * @see StructBase::get
         */
        template <typename Field>
        inline ValType<Field> get (Field) const
        {
            return StructBase::get(data, Field());
        }
        
        /**
         * Returns a const reference to a field.
         * @see StructBase::ref
         */
        template <typename Field>
        inline ConstRefType<Field> ref (Field) const
        {
            return StructBase::ref(data, Field());
        }
        
        /**
         * Reads and returns the current structure data as a Val.
         */
        inline operator Val () const
        {
            return MakeVal(data);
        }
        
    public:
        char const *data;
    };
};

/**
 * Macro for defining structures.
 * @see StructBase
 */
#define APRINTER_TSTRUCT(StructName, Fields) \
struct StructName : public APrinter::StructBase<StructName> { \
    APRINTER_TSTRUCT__ADD_END(APRINTER_TSTRUCT__FIELD_1 Fields) \
    using StructFields = APrinter::MakeTypeList< \
        APRINTER_TSTRUCT__ADD_END(APRINTER_TSTRUCT__LIST_0 Fields) \
    >; \
};

#define APRINTER_TSTRUCT__ADD_END(...) APRINTER_TSTRUCT__ADD_END_2(__VA_ARGS__)
#define APRINTER_TSTRUCT__ADD_END_2(...) __VA_ARGS__ ## _END

#define APRINTER_TSTRUCT__FIELD_1(FieldName, FieldType) \
struct FieldName : public APrinter::StructField<FieldType> {}; \
APRINTER_TSTRUCT__FIELD_2

#define APRINTER_TSTRUCT__FIELD_2(FieldName, FieldType) \
struct FieldName : public APrinter::StructField<FieldType> {}; \
APRINTER_TSTRUCT__FIELD_1

#define APRINTER_TSTRUCT__FIELD_1_END
#define APRINTER_TSTRUCT__FIELD_2_END

#define APRINTER_TSTRUCT__LIST_0(FieldName, FieldType) FieldName APRINTER_TSTRUCT__LIST_1
#define APRINTER_TSTRUCT__LIST_1(FieldName, FieldType) , FieldName APRINTER_TSTRUCT__LIST_2
#define APRINTER_TSTRUCT__LIST_2(FieldName, FieldType) , FieldName APRINTER_TSTRUCT__LIST_1

#define APRINTER_TSTRUCT__LIST_1_END
#define APRINTER_TSTRUCT__LIST_2_END

/**
 * Structure field type handler for integer types using
 * big-endian representaion.
 * These type handlers are registered by default for signed and
 * unsigned fixed-width types: intN_t and uintN_t (N=8,16,32,64).
 * 
 * Relies on ReadBinaryInt and WriteBinaryInt.
 */
template <typename Type>
struct StructBinaryTypeHandler {
    static size_t const FieldSize = sizeof(Type);
    
    using ValType = Type;
    
    inline static ValType get (char const *data)
    {
        return ReadBinaryInt<Type, BinaryBigEndian>(data);
    }
    
    inline static void set (char *data, ValType value)
    {
        WriteBinaryInt<Type, BinaryBigEndian>(value, data);
    }
};

#define APRINTER_STRUCT_REGISTER_BINARY_TYPE(Type) \
APRINTER_STRUCT_REGISTER_TYPE(Type, StructBinaryTypeHandler<Type>)

APRINTER_STRUCT_REGISTER_BINARY_TYPE(uint8_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(uint16_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(uint32_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(uint64_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(int8_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(int16_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(int32_t)
APRINTER_STRUCT_REGISTER_BINARY_TYPE(int64_t)

/**
 * Type handler for structure types, allowing nesting of structures.
 * 
 * It provides:
 * - get() and set() operations using StructType::Val.
 * - ref() operations using StructType::Ref and StructType::ConstRef.
 */
template <typename StructType>
struct StructNestedTypeHandler {
    static size_t const FieldSize = StructType::Size();
    
    using ValType = typename StructType::Val;
    using RefType = typename StructType::Ref;
    using ConstRefType = typename StructType::ConstRef;
    
    inline static ValType get (char const *data)
    {
        return StructType::MakeVal(data);
    }
    
    inline static void set (char *data, ValType value)
    {
        memcpy(data, value.data, sizeof(value.data));
    }
    
    inline static RefType ref (char *data)
    {
        return RefType{data};
    }
    
    inline static ConstRefType const_ref (char const *data)
    {
        return ConstRefType{data};
    }
};

template <typename Type>
struct StructTypeHandler<Type, EnableIf<__is_base_of(StructBase<Type>, Type), void>> {
    using Handler = StructNestedTypeHandler<Type>;
};

#include <aprinter/EndNamespace.h>

#endif
